(in-package #:rouse.viewer)


(defmethod compute-fovy ((sim sim:simulation))
  (coerce (* *params-ortho-fovy-scaling-factor*
	     (top:radius-of-gyration (sim:state-chain (sim:current-state sim))))
	  'single-float))

(defmethod ortho-initialize-camera ((sim sim:simulation) (os ortho-state))
  "Initialize an orthographic camera oriented according to the current face of `os`."
  (let ((face (ortho-get-face os)))
    (make-camera3d
     :position (first face)
     :target (vec 0.0 0.0 0.0)
     :up (second face)
     :fovy (compute-fovy sim)
     :projection :camera-orthographic)))



(defmethod ortho-set-camera-face ((os ortho-state) camera i)
  "Set the camera orientation to the i-th orthographic face."
  (let ((new-face (ortho-set-face os i)))
    (setf (camera3d-position camera) (first new-face))
    (setf (camera3d-up camera) (second new-face))))


(defmethod ortho-input ((sim sim:simulation) (os ortho-state)
			camera)
  "Handle keyboard input for camera control and time navigation in orthographic view."
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
    (sim:backward sim))

  (when (is-key-pressed :key-p)
    (sim:propagate sim)))



(defmethod ortho-draw ((sim sim:simulation) (os ortho-state)
		       camera)
  "Render the current simulation state and HUD using the orthographic camera."
  (let* ((state (sim:current-state sim))
         (chain (top:remove-center-of-mass (sim:state-chain state))))
    (with-drawing
      (clear-background :black)
      (setf (slot-value camera 'cl-raylib::fovy) (compute-fovy sim))
      (render-hud sim os camera)
      (with-mode-3d (camera)
        (render-chain chain)))))



(defmethod ortho-view-mode ((sim sim:simulation))
  "Run the orthographic view mode: handle input, update camera, and render frames in real time."
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
