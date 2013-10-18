(in-package #:cl-user)

(defpackage #:octopus-system
    (:use #:cl #:asdf))

(in-package #:octopus-system)

(defsystem octopus
    :name "octopus"
    :author "Jakub Kubiak"
    :version "0.1"
    :description "multiplayer 2d shooter game server"
    :depends-on
    (:clws :hu.dwim.defclass-star+hu.dwim.def :cl-json :alexandria)
    :serial t
    :components ((:file "package")
                 (:file "utils")
                 (:file "server")
                 (:file "octopus")))

