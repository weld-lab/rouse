(asdf:defsystem "rouse"
  :description "Solving Langevin dynamics for the Rouse model"
  :author "Erwan Le Doeuff (weld)"
  :license "MIT License"
  :serial t
  :in-order-to ((test-op (test-op "rouse/tests")))
  :components ((:file "package")
	       (:module "simulation"
		:components ((:file "package")
			     (:file "state")
			     (:file "simulation")))))


(asdf:defsystem "rouse/tests"
  :depends-on ("rouse" "fiveam")
  :components ((:file "tests/package")
	       (:file "tests/simulation"))
  :perform (test-op (o c)
	     (uiop:symbol-call :fiveam :run! :rouse-tests)))
