(in-package #:rouse.viewer)



(defmethod ortho-initialize-camera ((sim sim:simulation) (os ortho-state))
  (let ((face (ortho-get-face os)))
    (make-camera3d
     :position (first face)
     :target (vec 0.0 0.0 0.0)
     :up (second face)
     :fovy (* *params-ortho-fovy-scaling-factor*
	      (top:radius-of-gyration (sim:state-chain (sim:current-state sim))))
     :projection :camera-orthographic)))

(defmethod ortho-set-camera-face ((os ortho-state) camera i)
  (let ((new-face (ortho-set-face os i)))
    (setf (camera3d-position camera) (first new-face))
    (setf (camera3d-up camera) (second new-face))))

(defmethod ortho-input ((sim sim:simulation) (os ortho-state)
			camera)
  (when (is-key-pressed :key-space)
    (let ((new-face (ortho-go-next-face os)))
      (setf (camera3d-position camera) (first new-face))
      (setf (camera3d-up camera) (second new-face))))

  (when (is-key-pressed :key-one)
    (ortho-set-camera-face os camera 0))

  (when (is-key-pressed :key-two)
    (ortho-set-camera-face os camera 1))

  (when (is-key-pressed :key-three)
    (ortho-set-camera-face os camera 2))

  (when (is-key-pressed :key-four)
    (ortho-set-camera-face os camera 3))

  (when (is-key-pressed :key-five)
    (ortho-set-camera-face os camera 4))

  (when (is-key-pressed :key-six)
    (ortho-set-camera-face os camera 5))

  (when (is-key-pressed :key-right)
    (sim:forward sim))

  (when (is-key-pressed :key-left)
    (sim:backward sim)))



(defmethod hud-draw ((sim sim:simulation) (os ortho-state)
		       camera)
  (draw-text (third (ortho-get-face os))
	     10 0 30 :red)

  (draw-text (format nil "t=~a"
		     (-
		      (1- (length (sim:simulation-timeline sim)))
		      (sim:simulation-cursor sim)))
	     10 30 30 :blue))

(defmethod ortho-draw ((sim sim:simulation) (os ortho-state)
		       camera)
  (let* ((state (sim:current-state sim))
         (chain (top:remove-center-of-mass (sim:state-chain state))))
    (with-drawing
      (clear-background :black)
      (hud-draw sim os camera)
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
