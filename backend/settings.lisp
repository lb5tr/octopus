(in-package :octopus)

(defparameter *server-thread* nil)
(defparameter *resource-thread* nil)

(defparameter *port* 7878)
(defparameter *resource-path* "/api")
(defparameter *resource-listener-name* "octopus api")
(defparameter *server-thread-name* "octopus server thread")
(defparameter *resource-origin-prefixes* '(""))

;log options
(defparameter *log-sender-name* 'octopus-log)
(defparameter *log-location* "./octopus.log")

;database
(defparameter *database-host* "localhost")
(defparameter *database-user* "octopus")
(defparameter *database-name* "octopus")
(defparameter *database-password* nil)
