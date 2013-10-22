(in-package :octopus)

(def class* dummy ()
  ())

(def class* user-v ()
  ((username nil :type string)
   (password-hash nil :type string)))

(def class* client-message ()
  ((user-id nil :type string)
   (message-type "undefined" :type string)
   (payload nil)))

(def class* channel ()
  ((name nil :type string)
   (channel-locator nil :type string)
   (users (make-hash-table) :type hash-table)
   (map nil :type string)
   (capacity 0 :type integer)
   (players-count 0 :type integer)
   (password-hash nil :type string)
   (protected nil :type boolean)
   (creation-time (get-universal-time) :type date)
   (worker nil)))

;channel manager resource
(def class* channel-manager-resource (ws-resource)
  ())

(defmethod resource-client-connected ((res channel-manager-resource) client)
  (log-as info "client connected on channel-manager server from ~s : ~s" (client-host client) (client-port client))
  t)

(defmethod resource-client-disconnected ((resource channel-manager-resource) client)
  (log-as info "client disconnected from resource ~A" resource))

(defmethod resource-received-text ((res channel-manager-resource) client message)
  (log-as info "got frame ~s... from client ~s" (subseq message 0 10) client)
  (write-to-client-text client (dispatch-message (json-to-client-message message))))

(defmethod resource-received-binary((res channel-manager-resource) client message)
  (log-as info "got binary frame len: ~s" (length message) client)
  (write-to-client-binary client message))
