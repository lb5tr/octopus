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

(defun ensure-username-not-taken (name)
  (not (get-user-from-database-by-name name)))

(defun get-user-from-database-by-name (name)
  (with-transaction
    (select-instance (u user)
      (where (and
              (string= (username-of u) name))))))

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

(defun in-range (p tp)
  (let* ((a (getf tp :a))
         (b (getf tp :b))
         (x1 (getf a :x))
         (y1 (getf a :y))
         (x2 (getf b :x))
         (y2 (getf b :y))
         (mx (getf p :x))
         (my (getf p :y)))
    (and (>= mx x1) (>= my y1)
             (<= mx x2) (<= my y2))))

(defun check-for-score (ball channel)
  (with-slots ((scor score)) channel
    (with-slots ((pos position) (r radius)) ball
      (if (in-range pos `(:a (:x ,r :y 125) :b (:x ,(+ r 1) :y 225)))
          (progn
            (setf scor (cons (+ (car scor) 1) (cdr scor)))
            t)
          (if (in-range pos `(:a (:x ,(- 700 r 1) :y 125) :b (:x ,(- 700 r) :y 225)))
              (progn
                (setf scor (cons (car scor) (+ 1 (cdr scor))))
                t)
              nil)))))

(defun reset-positions (channel)
  (with-slots ((bi ball-instance) (us users)) channel
    (maphash (lambda (key user) (setf (position-of user) (starting-postion-of user))) us)
    (with-slots ((pos position) (vel v)) bi
        (setf pos '(:x 350 :y 175)
              vel 0))))

(defun generate-new-state (channel)
  (when (check-for-score (ball-instance-of channel) channel)
    (reset-positions channel))
  (let* ((users (users-of channel))
         (users-list (loop for user being the hash-values in users collect user))
         (users-positions nil)
         (balli (ball-instance-of channel))
         (score (score-of channel)))
         (collision-between (ball-instance-of channel) users-list)
        (maphash (lambda (x y) (collision-between y `(,balli) :mangle-first nil)) users)
      ;  (mapcar (lambda (x) (collision-between x users-list)) users-list)
    (setf (ball-instance-of channel) (next-position balli users-list))
    (setf users-positions (loop for user being the hash-values in users collect
                               (progn
                                 (setf (gethash (uid-of user)
                                                (users-of channel)) (next-position user users-list))
                                 `(:pos ,(position-of user) :rotation ,(direction-of user) :team ,(team-of user) :name ,(username-of user)))))
    (make-instance 'game-state :players users-positions :ball-instance balli :score-yellow (car score) :score-blue (cdr score))))

(defun make-state-broadcast (chan)
  (lambda ()
    (loop do
         (with-lock-held ((lock-of chan))
           (let ((state (generate-new-state chan))
                 (users (users-of chan)))
             (loop for user being the hash-values in users do
                  (write-to-client-text (socket-of user) (response-with
                                                          :message-type :state
                                                          :payload state)))))
         (sleep *rate*))))

(defun login (user-data uid client)
  (apply #'response-with
         (cond
           ((ensure-user-not-logged-in user-data)
            (log-as info "user ~A already exists" (username-of user-data))
            '(:message-type :error :error-type user-already-logged-in))
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
;            (setf (listener-of channel-data) (start-ws-resource (concatenate 'string "/" channel-name)
 ;                                                               '("")
  ;                                                              'channel-resource
;                                                             channel-name)
            (setf
                  (state-broadcast-of channel-data) (make-thread (make-state-broadcast channel-data))
                  (capacity-of channel-data) (parse-integer (capacity-of channel-data))
                  (ball-instance-of channel-data) (make-instance 'ball))
            (add-channel *server* channel-name channel-data)
            `(:message-type :ok :payload ,channel-data))
          `(:message-type :error :error-type ,code)))))

(defun introduce-new-user (channel user)
  (with-slots ((pc players-count)) channel
    (with-slots ((tea team)) user
      (if (oddp pc)
          (progn
            (setf tea 'blue)
            (incf (blue-team-count-of channel)))
          (progn
            (setf tea 'yellow)
            (setf (direction-of user) '(:x -1 :y 0 ))
            (incf (yellow-team-count-of channel))))
      (find-suitable-place-for user channel tea))))

(defun find-suitable-place-for (user channel team)
  (setf (starting-postion-of user) (cond
                                     ((equal team 'yellow) `(:x 450 :y ,(* (yellow-team-count-of channel) 50)))
                                      ((equal team 'blue)  `(:x 250 :y ,(* (blue-team-count-of channel) 50)))))
  (setf (position-of user) (starting-postion-of user)))

(defun find-user-by-socket (client)
  (block outter
    (loop for user being the hash-value of (users-by-uid-of *server*) when
         (eq (socket-of user) client) do
         (return-from outter user))))

(defun leave-channel (user)
  (let* ((channel (channel-of user))
         (lock (lock-of channel)))
    (with-lock-held (lock)
      (log-as :info "removing user ~A of uid ~A" user (uid-of user))
      (remhash (uid-of user) (users-of channel))
      (decf (players-count-of channel)))))

(defun leave-server (user)
  (with-slots ((ch channel)) user
    (cond
      ((equal (team-of user) 'blue) (decf (blue-team-count-of ch)))
      ((equal (team-of user) 'yellow) (decf (yellow-team-count-of ch)))))
  (remhash (uid-of user) (users-by-uid-of *server*))
  (remhash (username-of user) (users-by-username-of *server*)))

(defun dispose (client)
  (let ((user (find-user-by-socket client)))
    (when user
      (if (channel-of user)
          (leave-channel user))
      (leave-server  user))))

(def auth-handler leave (dummy uid client)
  (let ((user (get-user *server* uid :users-by 'users-by-uid-of)))
    (leave-channel user))
  '(:message-type :ok))

(def auth-handler join-channel (channel-data uid client)
  (let* ((user (get-user *server* uid :users-by 'users-by-uid-of))
         (channel-name (name-of channel-data))
         (channel (get-channel *server* channel-name))
         (players-count (players-count-of channel))
         (players (users-of channel))
         (given-password (password-hash-of channel-data))
         (channel-hash (password-hash-of channel))
         (capacity (capacity-of channel))
         (protectedp (protected-of channel))
         (lock (lock-of channel)))
    (if (and protectedp (not (string= given-password channel-hash)))
        (return-from handler '(:message-type :error :error-type wrong-password)))
    (if channel
        (if (< players-count capacity)
            (with-lock-held (lock)
              (log-as :info "User ~A joining ~A" (username-of user) channel-name)
              (setf (channel-of user) channel
                    (gethash uid players) user
                    (players-count-of channel) (incf players-count))
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

(defun try-to-kick (ball player)
  (let ((dd (direction-of player)))
    (if (<= (distance-between ball player) (+ *ball-radius* *player-radius* *kick-offset*))
        (progn
          (setf (direction-of ball) dd)
          (v-of ball) 6))))

;;TODO: remove duplications
(def auth-handler handle-player-event (event uid client)
  (let* ((player (get-user *server* uid :users-by 'users-by-uid-of))
         (channel (channel-of player))
         (position (position-of player))
         (event-type (event-type-of event)))
    (with-lock-held ((lock-of channel))
      (cond
        ((string= event-type "kick") (try-to-kick (ball-instance-of channel) player))
        ((string= event-type "left") (progn
                                        (decf (rof-of player)  0.5)
                                        (setf (direction-of player)
                                              (root-of-unity (rof-of player)))))
        ((string= event-type "right") (progn
                                        (incf (rof-of player) 0.5)
                                        (setf (direction-of player)
                                              (root-of-unity (rof-of player)))))
        ((string= event-type "up")   (setf (v-of player) (min 6 (+ (v-of player) 0.5))))
        ((string= event-type "down") (setf (v-of player) (max -6 (- (v-of player) 0.5))))))
    '(:message-type :ok)))

(defun create-new-user (name hash)
  (with-transaction
    (make-instance 'user :username name :password-hash hash)))

(defun register (user uid client)
  (apply 'response-with
    (with-slots ((name username) (pass password-hash)) user
        (if (ensure-username-not-taken name)
          (if (or (emptyp name) (emptyp pass))
              '(:message-type :error :error-type username-and-password-cannot-be-empty)
              (progn
                (create-new-user name (hash-string pass :digest *default-digest*))
               '(:message-type :ok)))
        '(:message-type :error :error-type username-taken)))))

;(defun ensure-no-collisions (et player channel)
;  (cond
;    ((string= event-type "left")  (check-for-collisons player channel '(:x -3 :y  0))))
;    ((string= event-type "right") (check-for-collisons player channel '(:x 3 :y  0))))
;    ((string= event-type "up")    (check-for-collisons player channel '(:x  0 :y -3))))
;    ((string= event-type "down")  (check-for-collisons player channel '(:x  0 :y  3))))))

;(defun check-for-collisons (p c offset)

;;client message mapping
(defparameter *message-type-alist*
  '(("undefined" . undefined-command)
    ("login" . login)
    ("leave" . leave)
    ("register" . register)
    ("list" . list-channels)
    ("create" . create-channel)
    ("get-channel" . get-channel)
    ("join" . join-channel)
    ("event" . handle-player-event)))

;;message to payload type
(defparameter *message-payload-alist*
  '((login . user-v)
    (leave . dummy)
    (register . user-v)
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
