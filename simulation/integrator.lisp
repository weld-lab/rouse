(in-package #:rouse.simulation)


(defun random-float (&key (precision 100))
  "Return a uniform random float in [0, 1)."
  (/ (random precision (make-random-state (not ctrl:*developer-mode*))) precision))


(defun box-muller ()
  "Return a normally distributed random number using the Box–Muller transform (mean 0, std 1)."
  (let* ((u1 (max (random-float) 1e-10))
         (u2 (random-float))
         (r (sqrt (* -2.0 (log u1))))
         (theta (* 2 pi u2)))
    (* r (cos theta))))



(defmethod interaction ((state state) i)
  "Harmonic potential between beads"
  (let* ((beads (top:chain-beads (state-chain state)))
         (N (length beads))
         (current (nth i beads))
         (pos (top:get-position current))
         (xi (first pos))
         (yi (second pos))
         (zi (third pos))
         (xprev xi) (yprev yi) (zprev zi)
         (xnext xi) (ynext yi) (znext zi))
    
    ;; left neighbor
    (when (> i 0)
      (let* ((bprev (nth (1- i) beads))
             (p (top:get-position bprev)))
        (setf xprev (first p)
              yprev (second p)
              zprev (third p))))
    
    ;; right neighbor
    (when (< i (1- N))
      (let* ((bnext (nth (1+ i) beads))
             (p (top:get-position bnext)))
        (setf xnext (first p)
              ynext (second p)
              znext (third p))))
    
    ;; return the Rouse force components
    (let ((k (state-k state)))
      (values (* -1 k (- (* 2 xi) xprev xnext))
              (* -1 k (- (* 2 yi) yprev ynext))
              (* -1 k (- (* 2 zi) zprev znext))))))

(defmethod euler-maruyama ((state state) &key (steps 1))
  "Propagate one Euler–Maruyama step for the Rouse model.
Integrates Langevin dynamics: dr = (F/gamma)*dt + sqrt(2*kB*T*dt/gamma)*N(0,1)."
  (let* ((new-state (copy-state state))
         (chain (state-chain new-state))
         (beads (top:chain-beads chain))
         (dt (state-dt state))
         (gamma (state-gamma state))
         (kB 1.0) ; à changer
         (temperature (state-temperature state))
         (sigma (sqrt (* 2 kB temperature dt (/ 1.0 gamma)))))  ;; √(2kTΔt/γ)
    
    (loop for i from 0 below (length beads)
          for bead = (nth i beads)
          do
             (multiple-value-bind (fx fy fz) (interaction new-state i)
               (destructuring-bind (x y z) (top:get-position bead)
                 (let ((dx (+ (* dt (/ fx gamma))
                              (* sigma (box-muller))))
                       (dy (+ (* dt (/ fy gamma))
                              (* sigma (box-muller))))
                       (dz (+ (* dt (/ fz gamma))
                              (* sigma (box-muller)))))
                   (setf (top:get-position bead)
                         (list (+ x dx)
                               (+ y dy)
                               (+ z dz)))))))
    
    (incf (state-time new-state) dt)
    new-state))
