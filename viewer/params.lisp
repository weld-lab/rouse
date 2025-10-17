(in-package #:rouse.viewer)


(defparameter *params-window-title* "Running...")
(defparameter *params-window-width* 600)
(defparameter *params-window-height* 600)
(defparameter *params-window-params*
  (list *params-window-width*
	*params-window-height*
	*params-window-title*))

(defparameter *params-log-level* :log-trace)
(defparameter *params-target-fps* 60)
(defparameter *params-ortho-fovy-scaling-factor* 5)



(defparameter *params-ortho-faces*
  (vector
   (list (vec  5.0  0.0  0.0) (vec 0.0 0.0 1.0) "+X")
   (list (vec -5.0  0.0  0.0) (vec 0.0 0.0 1.0) "-X")
   (list (vec  0.0  5.0  0.0) (vec 0.0 0.0 1.0) "+Y")
   (list (vec  0.0 -5.0  0.0) (vec 0.0 0.0 1.0) "-Y")
   (list (vec  0.0  0.0  5.0) (vec 0.0 1.0 0.0) "+Z")
   (list (vec  0.0  0.0 -5.0) (vec 0.0 1.0 0.0) "-Z")))
