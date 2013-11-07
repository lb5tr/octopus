(in-package :octopus)

(fiveam:def-suite basic :description "Basics")

(fiveam:in-suite basic)

(fiveam:test basic-tests
  "Login"
  (fiveam:is (string=
              (login (make-instance 'user-v :username "test-abc" :password-hash "badpassword") nil)
              "{\"messageType\":\"error\",\"payload\":{\"errorCode\":0,\"errorDescription\":\"NO-SUCH-USER\"}}")
             "Accepted user with bad password (or something else)"))
