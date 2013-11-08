(in-package :octopus)

(fiveam:def-suite basic :description "Basics")

(fiveam:in-suite basic)

(fiveam:test basic-tests
  "Login-should-not-pass"
  (fiveam:is (string=
              (login (make-instance 'user-v :username "test-abc" :password-hash "badpassword") nil)
              "{\"messageType\":\"error\",\"payload\":{\"errorCode\":0,\"errorDescription\":\"NO-SUCH-USER\"}}")
              "Accepted user with bad password (or something else)")
  "Login-should-pass"
  (fiveam:is (progn
               (let ((json (login (make-instance 'user-v
                                                 :username "test-user"
                                                 :password-hash (hash-string "abc" :digest *default-digest*)) nil)))
                 (get-user *server* "test-user" :users-by 'users-by-username-of)))))
