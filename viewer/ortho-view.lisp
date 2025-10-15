(in-package #:rouse.viewer)

(defmethod ortho-view-mode ((sim sim:simulation))
  (set-trace-log-level *params-log-level*)

  (let* ((faces (vector
                 (list (vec  5.0  0.0  0.0) (vec 0.0 0.0 1.0) "+X")
                 (list (vec -5.0  0.0  0.0) (vec 0.0 0.0 1.0) "-X")
                 (list (vec  0.0  5.0  0.0) (vec 0.0 0.0 1.0) "+Y")
                 (list (vec  0.0 -5.0  0.0) (vec 0.0 0.0 1.0) "-Y")
                 (list (vec  0.0  0.0  5.0) (vec 0.0 1.0 0.0) "+Z")
                 (list (vec  0.0  0.0 -5.0) (vec 0.0 1.0 0.0) "-Z")))
         (i 0))

    (let ((camera (make-camera3d
                   :position (first (aref faces i))
                   :target (vec 0.0 0.0 0.0)
                   :up (second (aref faces i))
                   :fovy 10.0
                   :projection :camera-orthographic)))

      (destructuring-bind (width height title) *params-window-params*
        (with-window (width height title)
          (set-target-fps *params-target-fps*)

          (loop until (window-should-close)
                do

		   ;; input
                   (when (is-key-pressed :key-space)
                     (setf i (mod (1+ i) (length faces)))
                     (setf (camera3d-position camera) (first (aref faces i)))
                     (setf (camera3d-up camera) (second (aref faces i)))
                     (format t "~&Switched to face ~A~%" (third (aref faces i))))

		   ;; draw
                   (let ((state (sim:current-state sim)))
                     (with-drawing
                       (clear-background :black)
                       (with-mode-3d (camera)
                         (render-chain (sim:state-chain state)))))))))))
