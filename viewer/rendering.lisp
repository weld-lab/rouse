(in-package #:rouse.viewer)


(defmethod scaled-position-vector ((bead top:bead))
  (apply #'vec (mapcar #'(lambda (x) (* *params-scaling-position* x))
				       (top:get-position bead))))


(defmethod render-bead ((bead top:bead) &key (color :gold) (radius 0.01))
  "Render a single bead as a colored sphere with black wireframe outlines"
  (let ((position (scaled-position-vector bead)))
    (draw-sphere position radius color)
    (draw-sphere-wires position radius 5 10 :black)))


(defmethod render-chain ((chain top:chain)
			 &key (bond-color :blue)
			   (bond-radius 0.08)
			   (bead-color :gold)
			   (bead-radius 0.5))
  "Render the polymer chain in 3D, displaying bonds as cylinders and beads as spheres"
  ;; draw bonds
  (let ((beads (top:chain-beads chain)))
    (loop for i from 0 below (1- (length beads))
          for bead1 = (nth i beads)
          for bead2 = (nth (1+ i) beads)
          for p1 = (scaled-position-vector bead1)
          for p2 = (scaled-position-vector bead2)
          do (draw-cylinder-ex p1 p2 bond-radius bond-radius 8 bond-color)))

  ;; draw beads
  (dolist (bead (top:chain-beads chain))
    (render-bead bead :color bead-color :radius bead-radius)))



(defmethod render-hud ((sim sim:simulation) (os ortho-state)
		       camera)
  "Render the on-screen HUD showing the current camera face and simulation time index"
  (draw-text (third (ortho-get-face os))
	     10 0 30 :red)
  (draw-text (format nil "step=~a"
		     (-
		      (1- (length (sim:simulation-timeline sim)))
		      (sim:simulation-cursor sim)))
	     10 30 30 :blue)
  (draw-text (format nil "t=~,3e s" (sim:state-time (sim:current-state sim)))
	     10 60 30 :green))
