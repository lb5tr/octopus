(in-package :octopus)

(def class* client-message ()
  ((connection-id nil :type string)
   (message-type "undefined" :type string)
   (payload nil)))

(def class* channel ()
  ((name nil :type string)
   (users (make-hash-table) :type hash-table)
   (current-map nil :type string)
   (creation-time (get-universal-time) :type date)))

