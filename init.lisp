(in-package #:rouse)


(defun start-engine ()
  (setf ctrl:*engine-running* t)
  (unless ctrl:*swank-thread*
    (setf ctrl:*swank-thread*
	  (bt:make-thread
	   (lambda ()
	     (swank:create-server :port 4005 :dont-close t)))))
  (loop while ctrl:*engine-running*
	do (let ((task (ctrl:recv-from-render-channel)))
	     (funcall task))))


(defun stop-engine ()
  (setf ctrl:*engine-running* nil)
  (when ctrl:*swank-thread*
    (ignore-errors
      (dolist (conn swank::*connections*)
        (swank::close-connection conn))
      (when swank::*listener-socket*
        (close swank::*listener-socket* :abort t)
        (setf swank::*listener-socket* nil))
      (sb-thread:terminate-thread ctrl:*swank-thread*) ; not portable
      (setf ctrl:*swank-thread* nil)))
  (sb-ext:exit :code 0))

(when (not ctrl:*developer-mode*)
  (start-engine))
