(in-package #:rouse.viewer)



(defmethod view ((sim sim:simulation) &key (mode :global-view))
  "Open a visualization window for the simulation in the selected mode (:global-view or :ortho-view)."
  (case mode
    (:global-view (ctrl:send-to-render-channel
		    (global-view-mode sim)))
    (:ortho-view  (ctrl:send-to-render-channel
                    (ortho-view-mode sim)))
    (t (error "Unknown view mode: ~A" mode))))
