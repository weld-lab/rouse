(in-package #:rouse.viewer)



(defclass ortho-state ()
  ((ortho-state-i :accessor ortho-state-i
                  :initform 0)
   (ortho-state-faces :accessor ortho-state-faces
                      :initform *params-ortho-faces*)))



(defmethod ortho-get-face ((os ortho-state))
  (aref (ortho-state-faces os) (ortho-state-i os)))

(defmethod ortho-set-face ((os ortho-state) i)
  (if (or (>= i (length (ortho-state-faces os))) (< i 0))
      (error "This face doesn't exist")
      (progn (setf (ortho-state-i os) i)
	     (ortho-get-face os))))

(defmethod ortho-go-next-face ((os ortho-state))
  (incf (ortho-state-i os))
  (when (>= (ortho-state-i os) (length (ortho-state-faces os)))
    (setf (ortho-state-i os) 0))
  (ortho-get-face os))
