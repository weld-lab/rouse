(in-package #:rouse.viewer)



(defmethod ortho-initialize-camera ((sim sim:simulation) (os ortho-state))
  (let ((face (ortho-get-face os)))
    (make-camera3d
     :position (first face)
     :target (vec 0.0 0.0 0.0)
     :up (second face)
     :fovy 10.0
     :projection :camera-orthographic)))



(defmethod ortho-input ((sim sim:simulation) (os ortho-state)
			camera)
  (when (is-key-pressed :key-space)
    (let ((new-face (ortho-go-next-face os)))
      (setf (camera3d-position camera) (first new-face))
      (setf (camera3d-up camera) (second new-face))
      (format t "~&Switched to face ~A~%" (third new-face)))))



(defmethod ortho-draw ((sim sim:simulation) (os ortho-state)
		       camera)
  (let* ((state (sim:current-state sim))
         (chain (top:remove-center-of-mass (sim:state-chain state))))
    (with-drawing
      (clear-background :black)
      (with-mode-3d (camera)
        (render-chain chain)))))



(defmethod ortho-view-mode ((sim sim:simulation))
  (set-trace-log-level *params-log-level*)
  (let* ((os (make-instance 'ortho-state))
         (camera (ortho-initialize-camera sim os)))
    (destructuring-bind (width height title) *params-window-params*
      (with-window (width height title)
        (set-target-fps *params-target-fps*)
        (loop until (window-should-close)
              do
		 (ortho-input sim os camera)
		 (ortho-draw  sim os camera))))))
