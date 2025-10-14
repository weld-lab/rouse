(in-package #:rouse.viewer)


(defmethod view ((sim sim:simulation)
		 &key (width 400) (height 400))
  (set-trace-log-level :log-trace)
    (let ((title "Running...")
	  (camera (make-camera3d :position (vec 1.0 0.0 0.0)
				 :target (vec 0.0 0.0 0.0)
				 :up (vec 0.0 0.0 1.0)
				 :fovy 60.0
				 :projection :camera-perspective)))
      (with-window (width height title)
	(set-target-fps 60)
	(loop until (window-should-close)
	      do (with-drawing
		   (clear-background :black)
		   (with-mode-3d (camera)
		     (draw-sphere (vec 0.0 0.0 0.0) 0.5 :gold)
		     (draw-sphere-wires (vec 0.0 0.0 0.0) 0.5 5 10 :black)))))))
