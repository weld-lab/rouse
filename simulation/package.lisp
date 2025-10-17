(defpackage #:rouse.simulation
  (:nicknames #:sim #:simulation)
  (:use #:cl)
  (:export #:make-simulation
	   #:simulation
	   #:simulation-cursor
	   #:simulation-timeline
	   #:current-state
	   #:forward
	   #:backward
	   #:add-to-timeline

	   #:state
	   #:state-chain))


(in-package #:rouse.simulation)
