(asdf:defsystem "rouse"
  :description "Solving Langevin dynamics for the Rouse model"
  :author "Erwan Le Doeuff (weld)"
  :license "MIT License"
  :in-order-to ((test-op (test-op "rouse/tests")))
  :serial t
  :depends-on ("cl-raylib" "3d-vectors" "bt-semaphore")
  :components ((:file "package")
	       (:module "simulation"
		:components ((:file "package")
			     (:file "state")
			     (:file "simulation")))
	       (:module "topology"
		:components ((:file "package")
			     (:file "bead")
			     (:file "chain")))
	       (:module "viewer"
		:components ((:file "package")
			     (:file "params")
			     (:file "rendering")
			     (:file "global-view")
			     (:file "ortho-view")
			     (:file "viewer")))))


(asdf:defsystem "rouse/tests"
  :depends-on ("rouse" "fiveam")
  :components ((:file "tests/package")
	       (:file "tests/topology")
	       (:file "tests/simulation")
	       (:file "tests/viewer"))
  :perform (test-op (o c)
	     (uiop:symbol-call :fiveam :run! :rouse-tests)))
