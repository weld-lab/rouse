(in-package #:rouse/tests)

(fiveam:in-suite :rouse-tests.topology)

(test macro-chain-creation
  (let ((chain (top:make-chain
		(:x 0.0 :y 0.0 :z 0.0)
		(:x 0.0 :y 1.0 :z 0.0 :vx 1.0))))
    (is (= (length (top::chain-beads chain))
	   2))))

(test copy-bead-and-chain
  (let* ((bead (top::make-bead :x 0.0))
	 (copied-bead (top::copy-bead bead)))
    (is (not (equal bead copied-bead)))
    (is (equal (top:get-position bead)
	       (top:get-position copied-bead)))
    (setf (top::bead-x bead) 1.0)
    (is (not (equal (top:get-position bead)
		    (top:get-position copied-bead)))))
  (let* ((chain (top:make-chain (:x 0.0)))
	 (copied-chain (top:copy-chain chain)))
    (is (not (equal chain copied-chain)))
    (is (= (length (top:chain-beads chain))
	   (length (top:chain-beads copied-chain))))
    (setf (top::bead-x (first (top:chain-beads copied-chain))) 1.0)
    (is (not (equal (top:chain-beads chain)
		    (top:chain-beads copied-chain))))))


(test chain-total-mass
      (let ((chain (top:make-chain (:mass 3.0)
				   (:mass 5.0))))
	(is (= 8.0 (top:total-mass chain)))))


(test chain-center-of-mass
  (let ((chain (top:make-chain
		(:x 1.0 :mass 3.0)
		(:x 0.0 :mass 2.0))))
    (is (equal '(0.6 0.0 0.0) (top:center-of-mass chain))))

  (let ((chain (top:make-chain
		(:x 1.0 :mass -1.0)
		(:x 0.0 :mass  1.0))))
    (signals division-by-zero (top:center-of-mass chain)))


  (let ((chain (top:make-chain
		(:x 1.0 :mass 1.0)
		(:x 1.0 :mass 2.0))))
    (is (equal '(0.0 0.0 0.0)
	       (top:center-of-mass
		(top:remove-center-of-mass chain))))))


(test chain-radius-of-gyration
  (let ((chain (top:make-chain (:mass 0.0))))
    (signals division-by-zero (top:radius-of-gyration chain)))

  (let ((chain (top:make-chain (:x 0.0) (:x 1.0))))
    (is (= 0.5 (top:radius-of-gyration chain)))))
