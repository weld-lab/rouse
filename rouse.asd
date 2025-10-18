(asdf:defsystem "rouse"
  :description "Solving Langevin dynamics for the Rouse model"
  :author "Erwan Le Doeuff (weld)"
  :license "MIT License"
  :in-order-to ((test-op (test-op "rouse/tests")))
  :serial t
  :depends-on ("swank" "chanl" "cl-raylib" "3d-vectors" "bt-semaphore")
  :components ((:file "package")
	       (:module "control"
		:components ((:file "package")
			     (:file "control")))
	       (:module "topology"
		:components ((:file "package")
			     (:file "bead")
			     (:file "chain")))
	       (:module "simulation"
		:components ((:file "package")
			     (:file "state")
			     (:file "simulation")
			     (:file "integrator")))
	       (:module "viewer"
		:components ((:file "package")
			     (:file "params")
			     (:file "ortho-state")
			     (:file "rendering")
			     (:file "global-view")
			     (:file "ortho-view")
			     (:file "viewer")))
	       (:file "init")))


(asdf:defsystem "rouse/tests"
  :depends-on ("rouse" "fiveam")
  :components ((:file "tests/package")
	       (:file "tests/topology")
	       (:file "tests/simulation")
	       (:file "tests/viewer"))
  :perform (test-op (o c)
	     (uiop:symbol-call :fiveam :run! :rouse-tests)))
