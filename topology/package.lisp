(defpackage #:rouse.topology
  (:nicknames #:top #:topology)
  (:use #:cl)
  (:export #:make-chain
	   #:chain
	   #:chain-beads
	   #:total-mass
	   #:remove-center-of-mass
	   #:center-of-mass
	   #:radius-of-gyration
	   
	   #:bead
	   #:get-position))

(in-package #:rouse.topology)
