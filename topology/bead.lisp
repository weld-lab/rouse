(in-package #:rouse.topology)


(defclass bead ()
  ((bead-x :accessor bead-x
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
