(defpackage :octopus
  (:use :cl :clws
        :cl-json
        :hu.dwim.defclass-star
        :log5
        :hu.dwim.def
        :hu.dwim.perec
        :fiveam
        :ironclad
        :closer-mop
        :alexandria
        :bordeaux-threads
        :anaphora)
  (:export :start-octopus-server)
  (:shadowing-import-from :hu.dwim.perec :set :time :null)
  (:shadowing-import-from :closer-mop :defgeneric
                          :standard-generic-function
                          :defmethod))
