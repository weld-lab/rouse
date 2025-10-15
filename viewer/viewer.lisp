(in-package #:rouse.viewer)


(defmethod view ((sim sim:simulation) &key (mode :global-view))
  (cond ((eq mode :global-view) (global-view-mode sim))
	((eq mode :ortho-view) (ortho-view-mode sim))
	(t (error "This view mode doesn't exist
Use :global-view or :ortho-view"))))
