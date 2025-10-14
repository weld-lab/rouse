(in-package #:rouse.viewer)


(defmethod ortho-view-mode ((sim sim:simulation))
  (set-trace-log-level *params-log-level*)
  (let ((camera (make-camera3d :position (vec -5.0 0.0 0.0)
			       :target (vec 0.0 0.0 0.0)
			       :up (vec 0.0 0.0 1.0)
			       :fovy 10.0
			       :projection :camera-orthographic)))

    (destructuring-bind (width height title) *params-window-params*
      (with-window (width height title)
	(set-target-fps *params-target-fps*)
	(loop until (window-should-close)
	      do (let ((state (sim:simulation-current-state sim)))
		   (with-drawing
		     (clear-background :black)
		     (with-mode-3d (camera)
		       (render-chain (sim:state-chain state))))))))))
