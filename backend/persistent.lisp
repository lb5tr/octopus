(in-package :octopus)

(defpclass* user ()
  ((username :type (text 256))
   (password-hash :type (text 256))))
