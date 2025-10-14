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
