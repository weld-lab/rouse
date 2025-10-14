(defpackage #:rouse.simulation
  (:nicknames #:sim #:simulation)
  (:use #:cl)
  (:export #:make-simulation
	   #:simulation
	   #:simulation-cursor
	   #:current-state

	   #:state
	   #:state-chain))


(in-package #:rouse.simulation)
