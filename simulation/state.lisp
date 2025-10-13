(in-package #:rouse.simulation)


(defclass state ()
  ((state-chain :accessor state-chain
		:initarg :state-chain
		:documentation "The polymer chain associated with this state.
It stores the positions of the beads and their connectivity
(see class `chain`).")
   
   (state-temperature :accessor state-temperature
		:initarg :state-temperature
		:initform 300.0
		:documentation "Thermal bath temperature (in Kelvin).
Controls the amplitude of the random force in the Langevin equation,
according to the fluctuation–dissipation theorem.")

   (state-gamma :accessor state-gamma
		:initarg :state-gamma
		:initform 1.0
		:documentation "Friction coefficient (or damping constant).
Determines the strength of the viscous drag on each bead.")

   (state-dt :accessor state-dt
	     :initarg :state-dt
	     :initform 1.0d-3
	     :documentation "Integration timestep (in seconds).
     Used to propagate the system forward in time via the Langevin update.")

   (state-time :accessor state-time 
	       :initarg :state-time
	       :initform 0.0
	       :documentation "Current simulation time (in seconds). 
This value is incremented after each call to the integrator (advance-state!)."))

  (:documentation
   "The STATE class represents a snapshot of the simulation at a given time.
It encapsulates both the polymer configuration (via `chain`) and the
environmental parameters that control its dynamics (temperature, friction, timestep).
A STATE instance is immutable by design: evolving the system in time should
produce a new STATE, rather than modifying the existing one."))


(defmacro make-state (&key chain temperature gamma dt time)
  `(make-instance 'state :state-chain ,chain
			 :state-temperature ,temperature
			 :state-gamma ,gamma
			 :state-dt ,dt
			 :state-time ,time))
