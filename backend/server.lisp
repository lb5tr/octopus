(in-package :octopus)

(defun start-octopus-server ()
  (initialize)
  (register-websocket-resource *channel-manager-resource-path*
                             *channel-manager-resource-origin-prefixes*
                             'channel-manager-resource)
  (start-websocket-server)
  (setf *channel-manager-resource-thread* (start-resource-listener
                                           *channel-manager-resource-path*
                                           *channel-manager-resource-listener-name*)))

(defun kill-octopus-server ()
  (bordeaux-threads:destroy-thread *channel-manager-resource-thread*)
  (bordeaux-threads:destroy-thread *server-thread*))

(defun restart-octopus-server ()
  (kill-octopus-server)
  (start-octopus-server))

(defun initialize ()
  ;put here initialization of all components
  (initialize-log))

(defun register-websocket-resource (path prefixes resource-class)
  (register-global-resource path
                          (make-instance resource-class)
                          (apply 'origin-prefix prefixes)))

(defun start-websocket-server ()
  (setf *server-thread* (bordeaux-threads:make-thread (lambda ()
				  (run-server *port*))
				:name *server-thread-name*)))

(defun start-resource-listener (path name)
  (bordeaux-threads:make-thread (lambda ()
                                  (run-resource-listener
                                   (find-global-resource path)))
                                :name name))
