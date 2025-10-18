(in-package #:rouse/tests)


(fiveam:in-suite :rouse-tests.simulation)


(test create-simulation-check-initial-state
  (let* ((sim (sim:make-simulation :chain nil
				   :temperature 1
				   :gamma 0
				   :dt 1.0d-3))
	 (state (sim:current-state sim)))
    (is (= (sim::state-temperature state) 1))))



(test simulation-timeline
  (let ((sim (sim:make-simulation :temperature 0 :gamma 0 :dt 0
	      :chain (top:make-chain (:x 0)))))
    (is (= 0 (sim:forward sim)))
    (is (= 0 (sim:backward sim)))

    (let ((state (sim::make-state
		  :temperature 0 :gamma 0 :dt 0 :time 1
		  :chain (top:make-chain (:x 1)))))
      (sim:add-to-timeline sim state)
      (is (= 1 (sim:simulation-cursor sim)))
      (is (= 0 (sim:forward sim))))))


(test copy-state
  (let* ((state (sim::make-state :chain (top:make-chain (:x 0.0))
		       :temperature 0
		       :gamma 0
		       :dt 0
		       :time 0))
	 (copied-state (sim:copy-state state)))
    (is (not (equal state copied-state)))
    (is (= (sim:state-temperature state)
	   (sim:state-temperature copied-state)))))
