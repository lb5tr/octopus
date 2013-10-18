(in-package :octopus)

(defparameter *server-thread* nil)
(defparameter *resource-thread* nil)

(defparameter *port* 7878)
(defparameter *resource-path* "/api")
(defparameter *resource-listener-name* "octopus api")
(defparameter *server-thread-name* "octopus server thread")
(defparameter *resource-origin-prefixes* '(""))
