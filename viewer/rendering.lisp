(in-package #:rouse.viewer)


(defmethod render-bead ((bead top:bead) &key (color :gold) (radius 0.5))
  (let ((position (apply #'vec (top:get-position bead))))
    (draw-sphere position radius color)
    (draw-sphere-wires position radius 5 10 :black)))


(defmethod render-chain ((chain top:chain)
			 &key (bond-color :blue)
			   (bond-radius 0.08)
			   (bead-color :gold)
			   (bead-radius 0.5))

  ; draw bonds
  (let ((beads (top:chain-beads chain)))
    (loop for i from 0 below (1- (length beads))
          for bead1 = (nth i beads)
          for bead2 = (nth (1+ i) beads)
          for p1 = (apply #'vec (top:get-position bead1))
          for p2 = (apply #'vec (top:get-position bead2))
          do (draw-cylinder-ex p1 p2 bond-radius bond-radius 8 bond-color)))

  ; draw beads
  (dolist (bead (top:chain-beads chain))
    (render-bead bead :color bead-color :radius bead-radius)))
