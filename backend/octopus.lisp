(in-package :octopus)

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
                      :payload (apply #'make-instance payload-class                           (alist-plist (assoc-cdr :payload alist)))))))

