;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Dec 05 10:29:28 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; shaders and code from https://github.com/vydd/sketch.git (cl-sdl2)
;;;
(in-package #:sketch)

#+cl-sdl2
(error "Wrong room")

;; see sdl2-sketch-window
(defclass sketch-app (gficl-app:base-app-bt)
  ((%sketch
    :initarg :sketch
    :accessor %sketch
    :initform nil
    :documentation "The sketch associated with this window."))
  (:default-initargs
   :context-version-major 3
   :context-version-minor 3
   :samples 4
   :aux-buffers 1
   :opengl-debug-context t
   ;; :disable-draw-fn t
   :opengl-profile :opengl-core-profile
   :height *default-height*
   :width *default-height*))


(defmethod gficl-app:cleanup-fn ((app sketch-app))
  ;; see close-window :before sdl2-sketch-window
  (with-slots ((instance %sketch)) app
    (with-environment (slot-value (%sketch app) '%env)
      (loop for resource being the hash-values of (env-resources *env*)
	    do (free-resource resource)))))

(defun %prepare (sketch)
  (check-type sketch sketch)
  (with-slots (%sketch-init-args) sketch
    (apply #'prepare sketch %sketch-init-args)
    ;; These will have been added in the call to PREPARE.
    (with-slots ((fs %delayed-init-funs)) sketch
      (loop for f across fs
	    do (funcall f))
      (setf fs (make-array 0 :adjustable t :fill-pointer t)))))

(defmethod gficl-app:setup-fn ((app sketch-app))
  ;; see initialize-instance :after sdl2-sketch
  (with-slots ((sketch %sketch)) app
    (when sketch
      (initialize-environment sketch)
      (initialize-gl sketch)
      (%prepare sketch)))
  (with-slots ((sketch %sketch) gficl::height gficl::width) app
    ;; gficl::height and gficl::width are initial values for the
    ;; window dimensions which are used in gficl:start via
    ;; gficl-app:launch. so we set them in gficl-app:setup-fn
    (setq gficl::height (sketch-height sketch))
    (setq gficl::width (sketch-width sketch))))

(defmethod gficl-app:resize-fn ((app sketch-app) w h)
  (with-slots ((sketch %sketch)) app
    ;; gficl::height and gficl::width are initial values for the
    ;; window dimensions and do not track current window
    ;; dimensions. However sketch.height and sketch.width do, so we
    ;; set them in gficl-app:resize-fn and update the gl:viewport via
    ;; sketch protocols
    (setf (sketch-height sketch) h)
    (setf (sketch-width sketch) w)
    (with-slots (%viewport-changed) sketch
      (setq %viewport-changed t))
    (maybe-change-viewport sketch)))

(defmethod gficl-app:draw-fn ((app sketch-app))
  ;; see kit.sdl2:render sdl2-sketch-window
  (with-slots ((sketch %sketch)) app
    (maybe-change-viewport sketch)
    (with-sketch (sketch)
      (with-gl-draw
	(with-error-handling (sketch)
          (unless (sketch-copy-pixels sketch)
            (background (gray 0.4)))
          (when (or (env-red-screen *env*)
                    (not (sketch-%setup-called sketch)))
            (setf (env-red-screen *env*) nil
                  (sketch-%setup-called sketch) t)
            (with-stage :setup
              (setup sketch)))
          (with-stage :draw
            (draw sketch)))))))

(defmethod gficl-app:update-fn ((app sketch-app))
  (with-slots ((instance %sketch)) app
    (alexandria:when-let (close-on (sketch-close-on instance))
      (gficl:map-keys-pressed (close-on (glfw:set-window-should-close))))))


;; we don't want (make-instance sketch) to launch the window, do that
;; through launch-sketch, which initializes sketch-window, which is a
;; gficl-app.  inializae-{gl,environment} is deferred to
;; gficl-app:setup-fn which is when we have an opegl context

(defmethod initialize-instance :after ((instance sketch) &rest initargs &key &allow-other-keys)
  (with-slots (%sketch-init-args) instance
    (setq %sketch-init-args initargs)))

(defun launch-sketch (instance &rest gficl-app-init-args &key &allow-other-keys)
  (check-type instance sketch)
  ;; TODO: support reinit of gficl-app window
  (unless (sketch-%window instance)
    (setf (sketch-%window instance)
	  (apply #'make-instance 'sketch-app
		 :title (sketch-title instance)
		 :width (sketch-width instance)
		 :height (sketch-height instance)
		 :resizable (sketch-resizable instance)
		 :sketch instance
		 gficl-app-init-args)))
  (apply #'gficl-app:launch (sketch-%window instance) gficl-app-init-args)
  (sketch-%window instance))

#||
(defclass sketch-eg0 (sketch) ())
(defmethod prepare ((instance sketch-eg0) &key &allow-other-keys))

(setq $sketch (make-instance 'sketch-eg0 ))
(launch-sketch $sketch :disable-draw-fn t)
(let* ((sketch $sketch)
       (app (sketch-%window sketch)))
  (gficl-app:in-thread app
    (maybe-change-viewport sketch)
    (with-sketch (sketch)
      (with-pen (make-pen :fill (rgb 0.380 0.695 0.086) :stroke (rgb 1 1 0) :weight 4)
	(with-gl-draw
	  (DRAW-SHAPE :TRIANGLE-STRIP
		      '((100 300) (100 100) (300 300) (300 100))
		      '((100 100) (100 300) (300 300) (300 100))))))
    (glfw:swap-buffers)))
(gficl-app:shutdown (sketch-%window $sketch))

(defsketch hello-world
    ((title "Hello, world!")
     (unit (/ width 10))
     (height width))
  (background (gray 0.6))
  (with-pen (make-pen :fill (rgb 0.380 0.695 0.086) :stroke (rgb 1 1 0) :weight 4)
    (polygon (* 5 unit) unit unit (* 9 unit) (* 9 unit) (* 9 unit))
    (text title 20 20)))

(setq $h (make-instance 'hello-world))
(launch-sketch $h :disable-draw-fn nil)
||#
