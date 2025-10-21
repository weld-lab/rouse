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

(defparameter *params-scaling-position* 3d9)
(defparameter *params-input-user-scaling* 1)

(defparameter *params-ortho-distance* 130.0)
(defparameter *params-ortho-faces*
  (vector
   (list (vec *params-ortho-distance* 0.0 0.0)      (vec 0.0 0.0 1.0) "+X")
   (list (vec (- *params-ortho-distance*) 0.0 0.0)  (vec 0.0 0.0 1.0) "-X")
   (list (vec 0.0  *params-ortho-distance* 0.0)     (vec 0.0 0.0 1.0) "+Y")
   (list (vec 0.0 (- *params-ortho-distance*) 0.0)  (vec 0.0 0.0 1.0) "-Y")
   (list (vec 0.0  0.0  *params-ortho-distance*)    (vec 0.0 1.0 0.0) "+Z")
   (list (vec 0.0  0.0 (- *params-ortho-distance*)) (vec 0.0 1.0 0.0) "-Z")))
