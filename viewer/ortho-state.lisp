(in-package #:rouse.viewer)



(defclass ortho-state ()
  ((ortho-state-i :accessor ortho-state-i
                  :initform 0)
   (ortho-state-faces :accessor ortho-state-faces
                      :initform *params-ortho-faces*))
  (:documentation "Hold the current state of the orthographic camera, including active face index and available view faces."))



(defmethod ortho-get-face ((os ortho-state))
  "Return the current active camera face from the orthographic state."
  (aref (ortho-state-faces os) (ortho-state-i os)))



(defmethod ortho-set-face ((os ortho-state) i)
  "Set the current camera face by index and return it, or signal an error if the index is invalid."
  (if (or (>= i (length (ortho-state-faces os))) (< i 0))
      (error "This face doesn't exist")
      (progn (setf (ortho-state-i os) i)
	     (ortho-get-face os))))



(defmethod ortho-go-next-face ((os ortho-state))
  "Switch to the next camera face in the orthographic state, cycling back to the first when reaching the end."
  (incf (ortho-state-i os))
  (when (>= (ortho-state-i os) (length (ortho-state-faces os)))
    (setf (ortho-state-i os) 0))
  (ortho-get-face os))
