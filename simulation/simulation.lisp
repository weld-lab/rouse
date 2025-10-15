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
  (error "Propagate is not yet implemented"))

(defmethod bifurcate ((sim simulation))
  (error "Bifurcate is not yet implemented"))
