(defpackage #:rouse.topology
  (:nicknames #:top #:topology)
  (:use #:cl)
  (:export #:make-chain
	   #:make-linear-chain
	   #:chain
	   #:chain-beads
	   #:copy-chain
	   #:total-mass
	   #:remove-center-of-mass
	   #:center-of-mass
	   #:radius-of-gyration
	   
	   #:bead
	   #:get-position
	   #:bead-mass))

(in-package #:rouse.topology)
