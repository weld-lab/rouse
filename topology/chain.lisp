(in-package #:rouse.topology)


(defclass chain ()
  ((chain-beads :accessor chain-beads
		:initarg :chain-beads
		:initform '())))


(defmacro make-chain (&rest bead-specs)
  `(make-instance 'chain
     :chain-beads
     (nreverse  (list ,@(mapcar (lambda (spec)
				  `(make-bead ,@spec))
				bead-specs)))))


(defmethod center-of-mass ((chain chain))
  (let* ((beads (chain-beads chain))
         (sum-x 0.0)
         (sum-y 0.0)
         (sum-z 0.0)
         (total-mass 0.0))
    
    (dolist (bead beads)
      (let ((m (bead-mass bead)))
        (incf sum-x (* m (bead-x bead)))
        (incf sum-y (* m (bead-y bead)))
        (incf sum-z (* m (bead-z bead)))
        (incf total-mass m)))
    
    (if (zerop total-mass)
        (list 0.0 0.0 0.0)
        (list (/ sum-x total-mass)
              (/ sum-y total-mass)
              (/ sum-z total-mass)))))
