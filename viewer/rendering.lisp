(in-package #:rouse.viewer)


(defmethod render-bead ((bead top:bead) &key (color :gold) (radius 0.5))
  (let ((position (apply #'vec (top:get-position bead))))
    (draw-sphere position radius color)
    (draw-sphere-wires position radius 5 10 :black)))


(defmethod render-chain ((chain top:chain) &key (bead-color :gold)
					     (bead-radius 0.5))
  (dolist (bead (top:chain-beads chain))
    (render-bead bead :color bead-color :radius bead-radius)))
