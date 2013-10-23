(in-package :octopus)

(defun undefined (payload uid) "undef")

(defun login (user-data uid)
  (let ((user (with-transaction
		(select-instance (u user)
		  (where (and
			  (string= (username-of u)
				   (username-of user-data))
			  (string= (password-hash-of u)
				   (password-hash-of user-data))))))))
    (if (equal user nil)
	(response-with :message-type :error
		       :error-code 0)
	(response-with :message-type :ok
		       :payload (next-random-uid)))))

(defun list-channels (payload uid) "list")

(defun create-channel (payload uid) "create")

(defun response-with (&key message-type (payload nil) (error-code -1))
  (encode-json-to-string
   (if (eq message-type :ok)
       (make-instance 'server-message
		      :message-type message-type
		      :payload payload)
       (make-instance 'server-message
		      :message-type message-type
		      :payload (make-instance 'error-payload
					      :error-code error-code
					      :error-description (assoc-cdr error-code
									    *error-codes*))))))

;client message mapping
(defparameter *message-type-alist*
  `(("undefined" . ,#'undefined)
    ("login" . ,#'login)
    ("list" . ,#'list-channels)
    ("create" . ,#'create-channel)))

;message to payload type
(defparameter *message-payload-alist*
  `((,#'login . user-v)
    (,#'undefined . dummy)
    (,#'list-channels . dummy)
    (,#'create-channel . channel)))

(defun initialize-uid-generator ()
  (setf *uid* (random-string)))

(defun next-random-uid ()
  (setf *uid* (hash-string *uid* :digest *default-digest*)))

(defun json-to-client-message (json)
  (let* ((alist (decode-json-from-string json))
         (msg-type-string (assoc-cdr :type alist))
         (user-id (assoc-cdr :user-id alist))
         (msg-class (assoc-cdr msg-type-string *message-type-alist*
                               :test #'equal))
         (payload-class (assoc-cdr msg-class *message-payload-alist*
                                   :test #'equal)))
    (if (or (not msg-type-string) (not msg-class) (not payload-class))
       (make-instance 'client-message :message-type 'undefined)
       (make-instance 'client-message
                      :message-type msg-class
                      :user-id user-id
                      :payload (apply #'make-instance payload-class
				      (alist-plist (assoc-cdr :payload alist)))))))

(defun dispatch-message (client-msg)
  (let ((msg-function (message-type-of client-msg))
	(payload (payload-of client-msg))
	(user-id (user-id-of client-msg)))
    (log-as info "dispatching ~A" msg-function)
    (funcall msg-function payload user-id)))

