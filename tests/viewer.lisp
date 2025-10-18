(in-package #:rouse/tests)


(fiveam:in-suite :rouse-tests.viewer)


(test view-mode-not-existing
  (let ((sim (sim:make-simulation :chain nil
				  :temperature 0
				  :k 0
				  :gamma 0
				  :dt 0)))
    (signals error
      (view:view sim :mode :not-existing-mode))))
