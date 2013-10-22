(in-package :octopus)

(def class* client-message ()
  ((connection-id nil :type string)
   (message-type "undefined" :type string)
   (payload nil)))

(def class* channel ()
  ((name nil :type string)
   (users (make-hash-table) :type hash-table)
   (current-map nil :type string)
   (creation-time (get-universal-time) :type date)
   (worker nil)))

;channel manager resource
(def class* channel-manager-resource (ws-resource)
  ())

(defmethod resource-client-connected ((res channel-manager-resource) client)
  (log-as info "client connected on channel-manager server from ~s : ~s" (client-host client) (client-port client))
  t)

(defmethod resource-client-disconnected ((resource channel-manager-resource) client)
  (log-as :info "client disconnected from resource ~A" resource))

(defmethod resource-received-text ((res channel-manager-resource) client message)
  (log-as info "got frame ~s... from client ~s" (subseq message 0 10) client)
  (write-to-client-text client message))

(defmethod resource-received-binary((res channel-manager-resource) client message)
  (log-as info "got binary frame len: ~s" (length message) client)
  (write-to-client-binary client message))