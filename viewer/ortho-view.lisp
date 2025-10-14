(in-package #:rouse.viewer)


(defmacro draw-polymer ()
  (draw-sphere (vec 0.0 -1.0 0.0) 0.5 :gold)
  (draw-sphere (vec 0.0 0.0 0.0) 0.5 :gold)
  (draw-sphere (vec 0.0 1.0 0.0) 0.5 :gold))

(defmethod ortho-view-mode ((sim sim:simulation) width height)
  (set-trace-log-level *params-log-level*)
  (let ((camera (make-camera3d :position (vec -5.0 0.0 0.0)
			       :target (vec 0.0 0.0 0.0)
			       :up (vec 0.0 0.0 1.0)
			       :fovy 10.0
			       :projection :camera-orthographic)))
    
    (with-window (width height *params-window-title*)
      (set-target-fps *params-target-fps*)
      (loop until (window-should-close)
	    do (with-drawing
		 (clear-background :black)
		 (with-mode-3d (camera)
		   (draw-sphere (vec 0.0 -2.0 0.0) 1.0 :gold)
		   (draw-sphere (vec 0.0 0.0 0.0) 1.0 :gold)
		   (draw-sphere (vec 0.0 2.0 0.0) 1.0 :gold)))))))
