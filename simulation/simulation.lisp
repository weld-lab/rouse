(in-package #:rouse.simulation)


(defclass simulation ()
  ((simulation-current-state :accessor simulation-current-state
			     :initarg :simulation-current-state
			     :documentation "The current STATE instance of the simulation. This represents the latest time step that has been computed.")

   (simulation-history :accessor simulation-history
		       :initform '()
		       :documentation "A list of past STATE instances.
Each element corresponds to a snapshot of the system at a previous time.
The most recent state is at the head of the list (LIFO order)."))

  (:documentation
   "The SIMULATION class manages the temporal evolution of the system.
It stores the current state (a STATE object) and a history of previous states.
This structure allows time navigation (forward, backward, branching) and
export of trajectories."))


(defmacro make-simulation (&key chain temperature gamma dt (time 0.0))
  `(make-instance 'simulation
		 :simulation-current-state (make-state :chain ,chain
						       :temperature ,temperature
						       :gamma ,gamma
						       :dt ,dt
						       :time ,time)))
