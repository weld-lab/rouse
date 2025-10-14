(in-package #:rouse/tests)

(fiveam:in-suite :rouse-tests.topology)

(test macro-chain-creation
  (let ((chain (top:make-chain
		(:x 0.0 :y 0.0 :z 0.0)
		(:x 0.0 :y 1.0 :z 0.0 :vx 1.0))))
    (is (= (length (top::chain-beads chain))
	   2))))
