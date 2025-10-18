(in-package #:rouse.control)



(defvar *swank-thread* nil)
(defvar *engine-running* nil)

(defvar *developer-mode* t)

(defvar *render-channel* (make-instance 'channel))


(defmacro send-to-render-channel (&body body)
  `(send *render-channel*
	 (lambda ()
	   ,@body)))

(defmacro recv-from-render-channel ()
  `(recv *render-channel*))
