(in-package #:rouse)


(defun start-engine ()
  (unless ctrl:*swank-thread*
    (setf ctrl:*swank-thread*
	  (bt:make-thread
	   (lambda ()
	     (swank:create-server :port 4005 :dont-close t)))))
  (loop
    (let ((task (ctrl:recv-from-render-channel)))
      (funcall task))))


(start-engine)
