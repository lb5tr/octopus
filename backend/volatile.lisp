(in-package :octopus)

(defun map-slots-with-ommit (function object)
  (let ((om (ommit (class-of object))))
    (loop for slot in (closer-mop:class-slots (class-of object))
       for slot-name = (closer-mop:slot-definition-name slot)
       if (and (slot-boundp object slot-name)
               (not (member slot-name om)))
       do (funcall function slot-name (slot-value object slot-name)))))

(def macro override-json-serialization (class-to-override)
  `(def method cl-json:encode-json ((self ,class-to-override) &optional stream)
     (with-object (stream)
       (map-slots-with-ommit (cl-json:stream-object-member-encoder stream) self))))

(def method sb-mop:validate-superclass ((self standard-class) s)
  t)

(def class* selective-serialization-class (standard-class)
  ((ommit-when-serializing nil :reader ommit)))

(def class* dummy ()
  ())

(def class* ball ()
  ((position (list :x 350 :y 175) :type list)
   (v 0 :type integer)
   (rof 0)
   (direction (root-of-unity 0) :type list)
   (radius 22 :type integer)))

(def class* user-v (ball)
  ((username nil :type string)
   (password-hash nil :type string)
   (uid nil :type string)
   (socket nil)
   (starting-postion 0)
   (team nil)
   (channel nil :type channel))
  (:metaclass selective-serialization-class)
  (:ommit-when-serializing password-hash admin-id worker lock socket channel))

(override-json-serialization user-v)

(def class* server-message ()
  ((message-type nil :type string)
   (payload nil)))

(def class* client-message ()
  ((uid nil :type string)
   (message-type "undefined" :type string)
   (payload nil)))

(def class* event ()
  ((event-type nil :type string)))

(def class* game-state ()
  ((players)
   (messeges nil)
   (score-yellow 0)
   (score-blue 0)
   (ball-instance :type ball)))

(def class* channel ()
  ((name nil :type string)
   (channel-locator nil :type string)
   (users (make-hash-table :test #'equal) :type hash-table)
   (map nil :type string)
   (admin-id nil :type string)
   (score '(0 . 0))
   (last-start-y 40)
   (blue-team-count 0)
   (yellow-team-count 0)
   (capacity :type integer)
   (ball-instance (make-instance 'ball) :type ball)
   (players-count 0 :type integer)
   (password-hash nil :type string)
   (protected nil)
   (creation-time (get-universal-time) :type date)
   (state-broadcast nil)
   (lock (make-lock)))
  (:metaclass selective-serialization-class)
  (:ommit-when-serializing password-hash admin-id score
                           lock state-broadcast listener))

(override-json-serialization channel)

(def class* server ()
  ((users-by-uid (make-hash-table :test #'equal) :type hash-table)
   (users-by-username (make-hash-table :test #'equal) :type hash-table)
   (channels (make-hash-table :test #'equal) :type hash-table)))

(def method add-channel ((srv server) name channel-data)
  (setf (gethash name (channels-of srv)) channel-data))

(def method get-channel ((srv server) name)
  (gethash name (channels-of srv)))

(def method add-user ((srv server) uid user-data)
  (setf (gethash uid (users-by-uid-of srv)) user-data)
  (setf (gethash (username-of user-data) (users-by-username-of srv)) user-data))

(def method get-user ((srv server) value &key users-by)
  (gethash value (funcall users-by srv)))

(def method rm-user ((srv server) value &key users-by)
  (let ((user (get-user srv value :users-by users-by)))
    (unless (eq user nil)
      (remhash (username-of user) (users-by-username-of srv))
      (remhash (uid-of user) (users-by-uid-of srv))
      t)))

(def class* error-payload ()
  ((error-code nil :type integer)
   (error-description nil :type string)))

(def method initialize-instance :after ((self error-payload) &rest args)
     (setf (error-description-of self) (symbol-name (getf args :error-description))))

;;channel manager resource
(def class* channel-manager-resource (ws-resource)
  ())

(defmethod resource-client-connected ((res channel-manager-resource) client)
  (log-as info "client connected on channel-manager server from ~s : ~s" (client-host client) (client-port client))
  t)

(defmethod resource-client-disconnected ((resource channel-manager-resource) client)
  (log-as info "client disconnected from resource ~A" resource)
  (dispose client))

(defmethod resource-received-text ((res channel-manager-resource) client message)
  (log-as info "got frame ~s... from client ~s" message client)
  (write-to-client-text client (dispatch-message (json-to-client-message message) :client client)))

(defmethod resource-received-binary((res channel-manager-resource) client message)
  (log-as info "got binary frame len: ~s" (length message) client)
  (write-to-client-binary client message))

