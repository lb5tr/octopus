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
(defparameter *log-location* "./octopus.log")

;database options
(defparameter *database-host* "localhost")
(defparameter *database-user* "octopus")
(defparameter *database-name* "octopus")
(defparameter *database-password* nil)

;client message mapping
(defparameter *message-type-alist*
  '(("undefined" . 'undefined)
    ("list" . 'list-channels)
    ("create" . 'create-channel)))

;message to payload type
(defparameter *message-payload-alist*
  '(('undefined . dummy)
    ('list-channels . dummy)
    ('create-channel . channel)))
