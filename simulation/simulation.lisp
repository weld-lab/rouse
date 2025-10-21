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



(defmethod propagate ((sim simulation) &key (steps 1) (save-every 1))
  "Integrate the system for STEPS iterations using Euler–Maruyama.
Intermediate states are saved every SAVE-EVERY steps; the final state is always saved.
If the cursor is in the past, future states are truncated before propagation."
  (dotimes (s steps)
    (let* ((timeline (simulation-timeline sim))
           (cursor (simulation-cursor sim))
           (n (length timeline)))
      (when (> cursor 0)
        (setf (simulation-timeline sim)
              (subseq timeline cursor n))
        (setf (simulation-cursor sim) 0)
        (setf timeline (simulation-timeline sim)))
      (let* ((current (current-state sim))
             (new-state (euler-maruyama current)))
	(when (or  (zerop (mod s save-every))
		   (= s (1- steps)))
          (push new-state (simulation-timeline sim))
          (setf (simulation-cursor sim) 0)))))
  sim)


(defmethod bifurcate ((sim simulation))
  (error "Bifurcate is not yet implemented"))
