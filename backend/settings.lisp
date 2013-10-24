(in-package :octopus)

;slots for threads
(defparameter *server-thread* nil)
(defparameter *channel-manager-resource-thread* nil)

;server options
(defparameter *port* 7878)
(defparameter *server-thread-name* "octopus server thread")

;channel manager WebSocket listner options
(defparameter *channel-manager-resource-path* "/channel-manager")
(defparameter *channel-manager-resource-listener-name* "octopus api")
(defparameter *channel-manager-resource-origin-prefixes* '(""))

;log options
(defparameter *log-sender-name* 'octopus-log)
(defparameter *log-location* *standard-output*)

;database options
(defparameter *database-host* "localhost")
(defparameter *database-user* "octopus")
(defparameter *database-name* "octopus")
(defparameter *database-password* nil)

;server error codes with descriptions
(defparameter *error-codes*
  '((0 . "No such user")))

;others
(defparameter *default-digest* 'sha1)
(defparameter *new-uid* (let ((uuid (random-string)))
			  (lambda ()
			    (setf uuid (hash-string uuid :digest *default-digest*)))))
