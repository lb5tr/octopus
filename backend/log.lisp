(in-package :octopus)

(defun initialize-log ()
  (log5:start-sender *log-sender-name*
		     (log5:stream-sender :location *log-location*)
		     :category-spec '(or info warn error)
		     :output-spec '(time "octopus:" log5:message)))

(defmacro log-as (category-spec message &rest args)
  `(log5:log-for (,category-spec) ,message ,@args))
