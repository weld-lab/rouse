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


(defmethod total-mass ((chain chain))
  (let ((total 0))
    (dolist (bead (chain-beads chain))
      (incf total (bead-mass bead)))
    total))


(defmethod center-of-mass ((chain chain))
  (let* ((beads (chain-beads chain))
         (sum-x 0.0)
         (sum-y 0.0)
         (sum-z 0.0)
         (total-mass (total-mass chain)))


    (when (zerop total-mass)
      (error 'division-by-zero
             :operation '/
             :operands (list 0 total-mass)))
    
    (dolist (bead beads)
      (let ((m (bead-mass bead)))
        (incf sum-x (* m (bead-x bead)))
        (incf sum-y (* m (bead-y bead)))
        (incf sum-z (* m (bead-z bead)))))

    (list (/ sum-x total-mass)
          (/ sum-y total-mass)
          (/ sum-z total-mass))))

(defmethod remove-center-of-mass ((chain chain))
  (let* ((com (center-of-mass chain))
         (cx (first com))
         (cy (second com))
         (cz (third com)))
    (make-instance 'chain
		   :chain-beads
		   (mapcar (lambda (bead)
			     (make-instance 'bead
					    :bead-x (- (bead-x bead) cx)
					    :bead-y (- (bead-y bead) cy)
					    :bead-z (- (bead-z bead) cz)
					    :bead-vx (bead-vx bead)
					    :bead-vy (bead-vy bead)
					    :bead-vz (bead-vz bead)
					    :bead-mass (bead-mass bead)))
			   (chain-beads chain)))))


(defmethod radius-of-gyration ((chain chain))
  (let* ((beads (chain-beads chain))
         (com (center-of-mass chain))
         (total-mass (total-mass chain))
         (sum 0.0))


    (when (zerop total-mass)
      (error 'division-by-zero
             :operation '/
             :operands (list 0 total-mass)))
    
    (dolist (bead beads)
      (let* ((m (bead-mass bead))
             (dx (- (bead-x bead) (first com)))
             (dy (- (bead-y bead) (second com)))
             (dz (- (bead-z bead) (third com))))
        (incf sum (* m (+ (* dx dx) (* dy dy) (* dz dz))))))
    
    (sqrt (/ sum total-mass))))
