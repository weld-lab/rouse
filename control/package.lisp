(defpackage #:rouse.control
  (:nicknames :control :ctrl)
  (:use #:cl #:chanl)
  (:export #:*swank-thread*
	   #:*engine-running*
	   #:*developer-mode*
	   #:*render-channel*
	   #:send-to-render-channel
	   #:recv-from-render-channel))


(in-package #:rouse.control)
