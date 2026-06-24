;;;; font.lisp

(in-package #:sketch)

;;;  _____ ___  _   _ _____
;;; |  ___/ _ \| \ | |_   _|
;;; | |_ | | | |  \| | | |
;;; |  _|| |_| | |\  | | |
;;; |_|   \___/|_| \_| |_|

(defclass font (resource)
  ((face :accessor font-face :initarg :face)
   (color :accessor font-color :initarg :color)
   (size :accessor font-size :initarg :size :initform 16)
   (line-height :accessor font-line-height :initarg :line-height :initform 1.41)
   (align :accessor font-align :initarg :align :initform :left)))

(defun make-font (&key face color size line-height align)
  (let* ((*env* (or *env* (make-env))))
    (make-instance 'font
                   :face (or face
                             (font-face (or (env-font *env*)
                                            (make-default-font))))
                   :color (or color +black+)
                   :size (coerce (truncate (or size 18))
                                 '(signed-byte 32))
                   :line-height (or line-height 1.41)
                   :align (or align :left))))

(defmacro with-font (font &body body)
  (with-shorthand (font make-font)
    (alexandria:with-gensyms (previous-font)
      `(let ((,previous-font (env-font *env*)))
         (unwind-protect (progn
                           (setf (env-font *env*) ,font)
                           ,@body)
           (setf (env-font *env*) ,previous-font))))))

(defun set-font (font)
  (setf (env-font *env*) font))

(defun text-scale (resources spacing width height)
  (let ((rendered-width (apply #'max (mapcar #'image-width resources)))
        (rendered-height (+ (* (- (length resources) 1) spacing)
                            (apply #'+ (mapcar #'image-height resources)))))
    (cond ((and (not (numberp width)) (not (numberp height))) (list 1 1))
          ((null width) (list 1 (if (zerop rendered-height) 1 (/ height rendered-height))))
          ((null height) (list (if (zerop rendered-width) 1 (/ width rendered-width)) 1))
          ((eq :keep-ratio width) (let ((scale (if (zerop rendered-height) 1 (/ height rendered-height))))
                                    (list scale scale)))
          ((eq :keep-ratio height) (let ((scale (if (zerop rendered-width) 1 (/ width rendered-width))))
                                     (list scale scale)))
          (t (list (if (zerop rendered-width) 1 (/ width rendered-width))
                   (if (zerop rendered-height) 1 (/ height rendered-height)))))))

(defun text-align (align width)
  (cond ((eq align :right) (- width))
        ((eq align :center) (- (round (/ width 2))))
        (t 0)))

#+cl-sdl2
(defun text-line-image (line)
  (let* ((line (if (> (length line) 0) line " "))
         (font (env-font *env*))
         (typeface (and font (load-resource (typeface-filename (font-face font))
                                            :size (font-size font)))))
    (destructuring-bind (r g b a) (color-rgba-255 (font-color font))
      (make-image-from-surface (sdl2-ttf:render-utf8-blended
                                (typeface-pointer typeface)
                                line r g b a)
                               :free-surface :font))))

#+glfwsketch
(defun text-line-image (line)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let* ((line (if (> (length line) 0) line " "))
         (font (env-font *env*))
         (typeface (and font (load-resource (typeface-filename (font-face font))
                                            :size (font-size font))))
	 (ft2-fbo-mixin (or (typeface-pointer typeface)
			    (setf (typeface-pointer typeface)
				  (%make-ft2-fbo-mixin-app
				   (typeface-filename typeface)
				   (font-size font)))))
	 (orig-program (gl:get-integer :current-program))
	 (orig-vertex-array (gl:get-integer :vertex-array-binding))
	 (orig-array-buffer (gl:get-integer :array-buffer-binding))
	 (orig-viewport (gl:get-float :viewport)))
    ;; env.resources is not clrhashed after sketch quits, but we
    ;; cleanup the fbo ft2-fbo-mixin-app object on quit. so
    ;; reinitialize
    (with-slots (gficl/load/ft2::fbo gficl/load/ft2::ft2) ft2-fbo-mixin
      (unless gficl/load/ft2::fbo
	(assert (null gficl/load/ft2::ft2))
	(gficl/load/ft2::ft2-fbo-mixin-app-setup ft2-fbo-mixin))
      (destructuring-bind (r g b a) (color-rgba-255 (font-color font))
	(gficl/load/ft2:ft2-app-set-text-color gficl/load/ft2::ft2
					       (/ r 255.0) (/ g 255.0)
					       (/ b 255.0)
					       (/ a 255.0)))
      (gl:clear-color 0 0 0 0)
      (let* ((source
	      (gficl/load/ft2:ft2-fbo-mixin-render-text-to-texture
		      ft2-fbo-mixin
		      line))
	     (h (slot-value ft2-fbo-mixin 'gficl/load/ft2::height))
	     (w (slot-value ft2-fbo-mixin 'gficl/load/ft2::width))
	     ;;#+nil
	     (tex (make-texture-rgba w h :source source
				     :wrap-s nil
				     :wrap-t nil
				     :min-filter :linear
				     :mag-filter :linear
				     )))
	;; ;madhu 260621 HORRIBLE HORRIBLE HORRIBLE, flip texture to
	;; fit sketch's broken automatic inversion, by consing up
	;; foreign memory for the image 60 times every second. TODO
	;; solve this by fixing the cl-sdl2 path to use opengl2 st
	;; coords for all textures (also undoing the fix in
	;; load-image-imlib2), or modify the fragment shader.lisp in
	;; shaders.lisp to use //f_out = texture(texid,
	;; vec2(f_texcoord.x, 1.0 - f_texcoord.y)) * f_color;
	(progn
	  (gl:bind-texture :texture-2d tex)
	  (cffi:with-foreign-object (data :uint8 (* w h 4))
	    (%gl:get-tex-image :texture-2d 0 :rgba :unsigned-byte data)
	    (GFICL/LOAD/IMAGE-IMLIB2::vertical-flip data w h)
	    (gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte data)))
	(prog1
	    #+nil
	  (make-instance 'image-no-free :width w :height h :texture tex)
	  ;;#+nil
	  (make-instance 'image :width w :height h :texture tex)
	  ;;(break)
	  ;;#+nil
	  (with-slots (%viewport-changed) *sketch*
	    (setq %viewport-changed t))
	  #+nil
	  (gl:viewport (elt orig-viewport 0) (elt orig-viewport 1)
		       (elt orig-viewport 2) (elt orig-viewport 3))
	  #+nil
	  (let ((program (gl:get-integer :current-program)))
	    (unless (= program orig-program)
	      #+nil
	      (format t "%text-line-program: setting current-program from ~D to ~D~&"
		      program  orig-program)
	      (gl:use-program orig-program)))
	  #+nil
	  (let ((vertex-array (gl:get-integer :vertex-array-binding)))
	    (unless (eql vertex-array orig-vertex-array)
	      (format t "%text-line-program: setting vertex-array from ~D to ~D~&"
		      vertex-array orig-vertex-array)
	      (%gl:bind-vertex-array orig-vertex-array)))
	  #+nil
	  (let ((array-buffer (gl:get-integer :array-buffer-binding)))
	    (unless (eql array-buffer orig-array-buffer)
	      (format t "%text-line-program: setting array-buffer from ~D to ~D~&"
		      array-buffer orig-array-buffer)
	      (gl:bind-buffer :array-buffer orig-array-buffer))))))))

(defun text (text-string x y &optional width height)
  ;;(declare (optimize (speed 0) (safety 3) (debug 3)))
  (let* ((font (env-font *env*)))
    (when (and font (> (length text-string) 0))
      (with-pen (make-pen :stroke nil)
        (let* ((top 0)
               (lines (split-sequence:split-sequence #\newline text-string))
               (resources (mapcar #'text-line-image lines))
               (spacing (* (font-size font) (font-line-height font)))
               (scale (text-scale resources spacing width height)))

	  #+nil ;; #+glfwsketch
	  (loop for i from 1 for res in resources
		do
		(gficl/load/image-imlib2:save-texture-to-file nil (format nil "/dev/shm/~D.png" i)
							      :id
							      (sketch::image-texture res)))
          (dolist (resource resources)
            (draw resource
		  :x
                  (+ x (text-align (font-align font) (* (first scale) (image-width resource))))
		  :y
                  (+ y top)
		  :width
                  (* (first scale) (image-width resource))
		  :height
                  (* (second scale) (image-height resource)))
            (incf top (* (second scale) spacing))
            (gl:delete-textures (list (image-texture resource)))))))))

(let ((font))
  (defun make-default-font ()
    (setf font (or font
                   (let ((filename (relative-path "res/sourcesans/SourceSansPro-Regular.otf")))
		     #+sbcl
		     (setq filename (namestring (probe-file filename)))
                     (make-font :face #+cl-sdl2 (make-instance 'typeface
                                                     :filename filename
                                                     :pointer (sdl2-ttf:open-font filename 18))
				#+glfwsketch (make-instance 'typeface
					       :filename filename
					       :pointer
					       (%make-ft2-fbo-mixin-app filename 18))
                                :color +black+
                                :size 18))))))

(let ((font))
  (defun make-error-font ()
    (setf font (or font
                   (let ((filename (relative-path "res/sourcesans/SourceSansPro-Regular.otf")))
		     #+sbcl
		     (setq filename (namestring (probe-file filename)))
                     (make-font :face (make-instance 'typeface
                                                     :filename filename
                                                     :pointer #+cl-sdl2 (sdl2-ttf:open-font filename 16)
						     #+glfwsketch
						     (make-instance 'typeface
						       :filename filename
						       :pointer
						       (%make-ft2-fbo-mixin-app filename 16)))
                                :color +white+
                                :size 16))))))

