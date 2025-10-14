(defpackage #:rouse.simulation
  (:nicknames #:sim #:simulation)
  (:use #:cl)
  (:export #:make-simulation
	   #:simulation
	   #:simulation-current-state
	   #:simulation-history))


(in-package #:rouse.simulation)
