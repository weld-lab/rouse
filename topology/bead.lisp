(in-package #:rouse.topology)


(defclass bead ()
  ((bead-mass :accessor bead-mass
	      :initarg :bead-mass)
   (bead-x :accessor bead-x
	   :initarg :bead-x)
   (bead-y :accessor bead-y
	   :initarg :bead-y)
   (bead-z :accessor bead-z
	   :initarg :bead-z)
   (bead-vx :accessor bead-vx
	    :initarg :bead-vx)
   (bead-vy :accessor bead-vy
	    :initarg :bead-vy)
   (bead-vz :accessor bead-vz
	    :initarg :bead-vz)))


(defmacro make-bead (&key (mass 1.0) (x 0.0) (y 0.0) (z 0.0) (vx 0.0) (vy 0.0) (vz 0.0))
  `(make-instance 'bead :bead-mass ,mass
		  :bead-x ,x :bead-y ,y :bead-z ,z
		  :bead-vx ,vx :bead-vy ,vy :bead-vz ,vz))



(defmethod copy-bead ((bead bead))
  "Deep copy of a bead"
  (make-bead :mass (bead-mass bead)
	     :x (bead-x bead)
	     :y (bead-y bead)
	     :z (bead-z bead)
	     :vx (bead-vx bead)
	     :vy (bead-vy bead)
	     :vz (bead-vz bead)))


(defmethod get-position ((bead bead))
  (list (bead-x bead) (bead-y bead) (bead-z bead)))



(defmethod (setf get-position) (new-pos (bead bead))
  "Set bead position (x y z) from a list of 3 numbers."
  (destructuring-bind (x y z) new-pos
    (setf (bead-x bead) x
          (bead-y bead) y
          (bead-z bead) z)
    new-pos))
