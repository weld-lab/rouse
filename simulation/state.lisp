(in-package #:rouse.simulation)


(defclass state ()
  ((state-chain :accessor state-chain
		:initarg :state-chain)
   (state-temperature :accessor state-temperature
		:initarg :state-temperature
		:initform 300.0)
   (state-k :accessor state-k
	    :initarg :state-k)
   (state-gamma :accessor state-gamma
		:initarg :state-gamma
		:initform 1.0)
   (state-dt :accessor state-dt
	     :initarg :state-dt
	     :initform 1.0d-3)
   (state-time :accessor state-time 
	       :initarg :state-time
	       :initform 0.0)))


(defmacro make-state (&key chain temperature k gamma dt time)
  `(make-instance 'state :state-chain ,chain
			 :state-temperature ,temperature
			 :state-k ,k
			 :state-gamma ,gamma
			 :state-dt ,dt
			 :state-time ,time))



(defmethod copy-state ((state state))
  "Deep copy of a state"
  (make-state :chain (top:copy-chain (state-chain state))
	      :k (state-k state)
	      :temperature (state-temperature state)
	      :gamma (state-gamma state)
	      :dt (state-dt state)
	      :time (state-time state)))
