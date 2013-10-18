(in-package :octopus)

(defun clws-debug (&key enable)
  (setf clws:*debug-on-server-errors* enable
	clws:*debug-on-resource-errors* enable))
