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
  '((unknown . -1)
    (no-such-user . 0)
    (user-already-logged-in . 1)
    (no-such-uid . 2)
    (user-not-authenticated . 3)
    (undefined-message-type . 4)
    (channel-already-exisits . 5)
    (invalid-name . 6)
    (user-already-at-channel . 7)
    (no-such-channel . 8)
    (full-channel . 9)
    (wrong-password . 10)
    (username-taken . 11)
    (username-and-password-cannot-be-empty . 12)))

;others
(defparameter *default-digest* 'sha1)
(defparameter *width* 700)
(defparameter *height* 350)
(defparameter *rate* 1/30)
(defparameter *ball-radius* 22)
(defparameter *kick-offset* 2)
(defparameter *player-radius* 22)
(defparameter *new-uid* (let ((uuid (random-string)))
                          (lambda ()
                            (setf uuid (hash-string uuid :digest *default-digest*)))))
