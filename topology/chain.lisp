(in-package #:rouse.topology)


(defclass chain ()
  ((chain-beads :accessor chain-beads
		:initarg :chain-beads
		:initform '())))


(defmacro make-chain (&rest bead-specs)
  `(make-instance 'chain
     :chain-beads
     (list ,@(mapcar (lambda (spec)
                       `(make-bead ,@spec))
                     bead-specs))))
