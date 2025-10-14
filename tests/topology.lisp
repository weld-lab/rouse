(in-package #:rouse/tests)

(fiveam:in-suite :rouse-tests.topology)

(test macro-chain-creation
  (let ((chain (top:make-chain
		(:x 0.0 :y 0.0 :z 0.0)
		(:x 0.0 :y 1.0 :z 0.0 :vx 1.0))))
    (is (= (length (top::chain-beads chain))
	   2))))


(test chain-center-of-mass
  (let ((chain (top:make-chain
		(:x 1.0 :mass 3.0)
		(:x 0.0 :mass 2.0))))
    (is (equal '(0.6 0.0 0.0) (top:center-of-mass chain))))

  (let ((chain (top:make-chain
		(:x 1.0 :mass -1.0)
		(:x 0.0 :mass  1.0))))
    (is (equal '(0.0 0.0 0.0) (top:center-of-mass chain)))))
