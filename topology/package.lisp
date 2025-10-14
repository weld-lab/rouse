(defpackage #:rouse.topology
  (:nicknames #:top #:topology)
  (:use #:cl)
  (:export #:make-chain
	   #:total-mass
	   #:remove-center-of-mass
	   #:center-of-mass
	   #:radius-of-gyration))

(in-package #:rouse.topology)
