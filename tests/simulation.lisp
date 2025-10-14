(in-package #:rouse/tests)


(fiveam:in-suite :rouse-tests.simulation)


(test create-simulation-check-initial-state
  (let* ((sim (sim:make-simulation :chain nil
				   :temperature 1
				   :gamma 0
				   :dt 1.0d-3))
	 (state (sim:current-state sim)))
    (is (= (sim::state-temperature state) 1))))
