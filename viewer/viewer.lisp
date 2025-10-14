(in-package #:rouse.viewer)


(defmethod view ((sim sim:simulation)
		 &key (width 400) (height 400) (mode :global-view))
  (cond ((eq mode :global-view) (global-view-mode sim width height))
	((eq mode :ortho-view) (ortho-view-mode sim width height))
	(t (error "This view mode doesn't exist
Use :global-view or :ortho-view"))))
