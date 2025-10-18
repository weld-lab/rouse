(in-package #:rouse.simulation)


(defclass simulation ()
  ((simulation-cursor :accessor simulation-cursor
		      :initform 0)
   (simulation-timeline :accessor simulation-timeline
			:initarg :simulation-timeline
			:initform '()))
  (:documentation "Represent a simulation, containing a timeline of states and a cursor pointing to the current one."))


(defmacro make-simulation (&key chain temperature k gamma dt (time 0.0))
  `(make-instance 'simulation
		  :simulation-timeline  (list (make-state :chain ,chain
							  :k ,k
							  :temperature ,temperature
							  :gamma ,gamma
							  :dt ,dt
							  :time ,time))))




(defmethod current-state ((sim simulation))
  (nth (simulation-cursor sim) (simulation-timeline sim)))


(defmethod bound ((sim simulation))
  (let ((cursor (simulation-cursor sim))
	(n (length (simulation-timeline sim))))
    (cond ((>= cursor n)
	   (setf (simulation-cursor sim) (1- n)))
	  ((< cursor 0)
	   (setf (simulation-cursor sim) 0)))
    (simulation-cursor sim)))



(defmethod move-cursor ((sim simulation) delta)
  (incf (simulation-cursor sim) delta)
  (bound sim))



(defmethod forward ((sim simulation) &key (steps 1))
  (move-cursor sim (- steps)))



(defmethod backward ((sim simulation) &key (steps 1))
  (move-cursor sim steps))


(defmethod add-to-timeline ((sim simulation) (state state))
  (push state (simulation-timeline sim))
  (backward sim))


(defmethod propagate ((sim simulation) &key (steps 1))
  "Advance the simulation by STEPS iterations using Euler–Maruyama.
Each step creates a new state, appends it to the timeline,
and removes any future states if the cursor was not at the end."
  (dotimes (s steps)
    ;; 1. Get current state
    (let* ((current (current-state sim))
           (new-state (euler-maruyama current))
           (timeline (simulation-timeline sim))
           (cursor (simulation-cursor sim)))

      ;; 2. If we were in the middle of the timeline, delete future states
      (when (< cursor (1- (length timeline)))
        (setf (simulation-timeline sim)
              (subseq timeline 0 (1+ cursor))))

      ;; 3. Push new state and update cursor
      (push new-state (simulation-timeline sim))
      (setf (simulation-cursor sim) 0)))  ; latest state is at the front

  sim)

;; (defmethod propagate ((sim simulation) &key (steps 1))
;;   (error "Propagate is not yet implemented"))

(defmethod bifurcate ((sim simulation))
  (error "Bifurcate is not yet implemented"))
