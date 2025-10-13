(in-package #:rouse.simulation)


(defclass state ()
  ((chain
    :accessor chain
    :initarg :chain
    :documentation
    "The polymer chain associated with this state.
     It stores the positions of the beads and their connectivity
     (see class `chain`).")
   
   (temperature
    :accessor temperature
    :initarg :temperature
    :initform 300.0
    :documentation
    "Thermal bath temperature (in Kelvin).
     Controls the amplitude of the random force in the Langevin equation,
     according to the fluctuation–dissipation theorem.")

   (gamma
    :accessor gamma
    :initarg :gamma
    :initform 1.0
    :documentation
    "Friction coefficient (or damping constant).
     Determines the strength of the viscous drag on each bead.")

   (dt
    :accessor dt
    :initarg :dt
    :initform 1.0d-3
    :documentation
    "Integration timestep (in seconds).
     Used to propagate the system forward in time via the Langevin update.")

   (time
    :accessor time
    :initarg :time
    :initform 0.0
    :documentation
    "Current simulation time (in seconds). This value is incremented
     after each call to the integrator (advance-state!)."))
  (:documentation
   "The STATE class represents a snapshot of the simulation at a given time.
It encapsulates both the polymer configuration (via `chain`) and the
environmental parameters that control its dynamics (temperature, friction, timestep).
A STATE instance is immutable by design: evolving the system in time should
produce a new STATE, rather than modifying the existing one."))
