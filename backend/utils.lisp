(in-package :octopus)

(defun clws-debug (&key enable)
  (setf clws:*debug-on-server-errors* enable
	clws:*debug-on-resource-errors* enable))

(defun hash-string (string &key digest)
  (byte-array-to-hex-string
   (digest-sequence digest
		    (ascii-string-to-byte-array string))))

(defun random-string (&rest random-salt-args)
  (byte-array-to-hex-string (apply #'make-random-salt random-salt-args)))

(defun assoc-cdr (&rest args)
  (cdr (apply #'assoc args)))
