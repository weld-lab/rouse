(asdf:defsystem "rouse"
    :description "Solving Langevin dynamics for the Rouse model"
    :author "Erwan Le Doeuff (weld)"
    :license "MIT License"
    :serial t
    :components ((:file "package")
		 (:module "simulation"
		  :components ((:file "package")))))


(asdf:defsystem "rouse/tests"
  :depends-on ("rouse" "fiveam")
  :components ((:file "tests/package")))
