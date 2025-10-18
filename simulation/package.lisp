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
	   #:propagate
	   #:add-to-timeline

	   #:state
	   #:state-chain
	   #:state-temperature
	   #:state-gamma
	   #:state-k
	   #:state-dt
	   #:state-time
	   #:copy-state))


(in-package #:rouse.simulation)
