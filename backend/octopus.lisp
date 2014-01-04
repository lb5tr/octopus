(in-package :octopus)

(def definer auth-handler (name (payload uid client) &body body)
  `(defun ,name (,payload ,uid ,client)
     (apply #'response-with (block handler (if (ensure-user-authenticated ,uid)
                                               (progn ,@body)
                                               '(:message-type :error :error-type user-not-authenticated))))))

(defparameter *server* (make-instance 'server))

(defun ensure-user-not-logged-in (user-data)
  (get-user *server*
            (username-of user-data)
            :users-by 'users-by-username-of))

(defun ensure-user-exists-in-database (user-data)
  (get-user-from-database user-data))

(defun get-user-from-database (user-data)
  (with-transaction
      (select-instance (u user)
                       (where (and
                               (string= (username-of u)
                                        (username-of user-data))
                               (string= (password-hash-of u)
                                        (password-hash-of user-data)))))))

(defun ensure-unique-channel (channel-name)
  (not (gethash channel-name (channels-of *server*))))

(defun ensure-user-authenticated (uid)
  (get-user *server* uid :users-by 'users-by-uid-of))

(defun ensure-proper-channel (channel-data)
  (let ((channel-name (name-of channel-data)))
    (if (emptyp channel-name)
        (values nil 'invalid-name)
        (if (ensure-unique-channel channel-name)
            (values t nil)
            (values nil 'channel-already-exisits)))))

(defun undefined-command (payload uid client)
  (response-with :message-type :error :error-type 'undefined-message-type))

(defun generate-new-state (channel)
  (let* ((users (users-of channel))
         (users-positions (loop for user being the hash-values in users collect
                               (position-of user)))
         (balli (ball-instance-of channel)))
    (setf (ball-instance-of channel) (next-position balli))
    (collision-between (ball-instance-of channel) (loop for user being the hash-values in users collect user
))
    (make-instance 'game-state :players users-positions :ball-instance balli)))

(defun make-state-broadcast (chan)
  (let ((channel chan))
    (lambda ()
      (loop do
           (with-lock-held ((lock-of channel))
             (let ((state (generate-new-state channel))
                   (users (users-of channel)))
               (loop for user being the hash-values in users do
                    (write-to-client-text (socket-of user) (response-with
                                                            :message-type :state
                                                            :payload state)))))
           (sleep *rate*)))))

(defun login (user-data uid client)
  (apply #'response-with
         (cond
           ;; ((ensure-user-not-logged-in user-data)
           ;;  (log-as info "user ~A already exists" (username-of user-data))
           ;;  '(:message-type :error :error-type user-already-logged-in))
           ((not (ensure-user-exists-in-database user-data))
            (log-as info "no such user ~A" (username-of user-data))
            '(:message-type :error :error-type no-such-user))
           (t (let ((new-uid (funcall *new-uid*)))
                (add-user *server* new-uid
                          (make-instance 'user-v
                                         :username (username-of user-data)
                                         :uid new-uid
                                         :socket client))
                `(:message-type :ok :payload ,new-uid))))))


(def auth-handler list-channels (payload uid client)
  `(:message-type :ok
                  :payload ,(channels-of *server*)))

(def auth-handler create-channel (channel-data uid client)
  (let ((channel-name (name-of channel-data))
        (password (password-hash-of channel-data)))
    (multiple-value-bind (ret code) (ensure-proper-channel channel-data)
      (if ret
          (progn
            (setf (admin-id-of channel-data) uid
                  (channel-locator-of channel-data) channel-name)
            (unless (emptyp password)
              (setf (protected-of channel-data) t))
            (setf (listener-of channel-data) (start-ws-resource (concatenate 'string "/" channel-name)
                                                                '("")
                                                                'channel-resource
                                                                channel-name)
                  (state-broadcast-of channel-data) (make-thread (make-state-broadcast channel-data))
                  (capacity-of channel-data) (parse-integer (capacity-of channel-data))
                  (ball-instance-of channel-data) (make-instance 'ball))
            (add-channel *server* channel-name channel-data)
            `(:message-type :ok :payload ,channel-data))
          `(:message-type :error :error-type ,code)))))

(defun introduce-new-user (channel user)
  (find-suitable-place-for user channel))

(defun find-suitable-place-for (user channel)
  (setf (position-of user) '(:x 200 :y 200)))

;;TODO:implement
(defun collisionsp (user)
  nil)

(def auth-handler join-channel (channel-data uid client)
  (let* ((user (get-user *server* uid :users-by 'users-by-uid-of))
         (channel-name (name-of channel-data))
         (channel(get-channel *server* channel-name))
         (players-count (players-count-of channel))
         (players (users-of channel))
         (capacity (capacity-of channel))
         (lock (lock-of channel)))
    (if channel
        (if (< players-count capacity)
            (with-lock-held (lock)
              (log-as :info "User ~A joining ~A" (username-of user) channel-name)
              (setf (channel-of user) channel
                    (gethash uid players) user
                    (players-count-of channel) (incf players-count)
                    (radius-of user) 5)
              (introduce-new-user channel user)
              (return-from handler `(:message-type :ok :payload ,channel)))
            (return-from handler '(:message-type :error :error-type full-channel)))
        (return-from handler '(:message-type :error :error-type no-such-channel)))))

(defun logout (payload uid client)
  (if (rm-user *server* uid :users-by 'users-by-uid-of)
      (response-with :message-type :ok)
      (response-with :message-type :error
                     :error-type 'no-such-uid)))

(defun response-with (&key message-type (payload nil) (error-type 'unknown))
  (encode-json-to-string
   (if (or (eq message-type :ok) (eq message-type :state))
       (make-instance 'server-message
                      :message-type message-type
                      :payload payload)
       (make-instance 'server-message
                      :message-type message-type
                      :payload (make-instance 'error-payload
                                              :error-code (assoc-cdr error-type
                                                                     *error-codes*)
                                              :error-description error-type)))))
;;TODO: remove duplications
(def auth-handler handle-player-event (event uid client)
  (let* ((player (get-user *server* uid :users-by 'users-by-uid-of))
         (channel (channel-of player))
         (position (position-of player))
         (event-type (event-type-of event)))
    (with-lock-held ((lock-of channel))
      (cond
        ((string= event-type "left") (setf (position-of player)
                                           (add-vectors position '(:x -3 :y  0))))
        ((string= event-type "right")(setf (position-of player)
                                           (add-vectors position '(:x 3 :y  0))))
        ((string= event-type "up")   (setf (position-of player)
                                           (add-vectors position '(:x  0 :y -3))))
        ((string= event-type "down") (setf (position-of player)
                                           (add-vectors position '(:x  0 :y  3))))))
    '(:message-type :ok)))

;;client message mapping
(defparameter *message-type-alist*
  '(("undefined" . undefined-command)
    ("login" . login)
    ("logout" . logout)
    ("list" . list-channels)
    ("create" . create-channel)
    ("join" . join-channel)
    ("event" . handle-player-event)))

;;message to payload type
(defparameter *message-payload-alist*
  '((login . user-v)
    (logout . dummy)
    (undefined-command . dummy)
    (list-channels . dummy)
    (create-channel . channel)
    (join-channel . channel)
    (handle-player-event . event)))

(defun json-to-client-message (json)
  (let* ((alist (decode-json-from-string json))
         (msg-type-string (assoc-cdr :message-type alist))
         (uid (assoc-cdr :uid alist))
         (msg-class (assoc-cdr msg-type-string *message-type-alist*
                               :test #'equal))
         (payload-class (assoc-cdr msg-class *message-payload-alist*
                                   :test #'equal)))
    (if (or (not msg-type-string) (not msg-class) (not payload-class))
        (make-instance 'client-message :message-type 'undefined)
        (make-instance 'client-message
                       :message-type msg-class
                       :uid uid
                       :payload (apply #'make-instance payload-class
                                       (alist-plist (assoc-cdr :payload alist)))))))

(defun dispatch-message (client-msg &key client)
  (let ((msg-function (message-type-of client-msg))
        (payload (payload-of client-msg))
        (uid (uid-of client-msg)))
    (log-as :info "dispatching ~A" msg-function)
    (funcall msg-function payload uid client)))
