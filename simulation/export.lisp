(in-package #:rouse.simulation)


(defmethod export-simulation ((sim simulation) prefix)
  "Export the simulation timeline to files:
 - <prefix>_positions.dat : bead coordinates over time
 - <prefix>_states.dat    : global state parameters over time"
  (let ((positions-file (format nil "~a-positions.dat" prefix))
        (states-file    (format nil "~a-states.dat" prefix))
        (timeline (reverse (simulation-timeline sim))))

    ;; save positions
    (with-open-file (pos positions-file
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format pos "time pos-in-chain mass x y z~%")
      (loop for state in timeline
            for current-time = (state-time state)
            do (loop for i from 0
                     for bead in (top:chain-beads (state-chain state))
                     do (destructuring-bind (x y z) (top:get-position bead)
			  (format pos "~e ~d ~e ~e ~e ~e~%"
				  current-time i (top:bead-mass bead) x y z)))))

    ;; save parameters
    (with-open-file (st states-file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
      (format st "time temperature gamma k dt~%")
      (loop for state in timeline
            do (format st "~f ~f ~f ~f ~f~%"
                       (state-time state)
                       (state-temperature state)
                       (state-gamma state)
                       (state-k state)
                       (state-dt state))))))
