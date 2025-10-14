(in-package #:rouse.simulation)


(defclass simulation ()
  ((simulation-cursor :accessor simulation-cursor
		      :initform 0)
   (simulation-timeline :accessor simulation-timeline
			:initarg :simulation-timeline
			:initform '())))


(defmacro make-simulation (&key chain temperature gamma dt (time 0.0))
  `(make-instance 'simulation
		  :simulation-timeline  (list (make-state :chain ,chain
							  :temperature ,temperature
							  :gamma ,gamma
							  :dt ,dt
							  :time ,time))))


(defmethod current-state ((simulation simulation))
  (nth (simulation-cursor simulation) (simulation-timeline simulation)))
