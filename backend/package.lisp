(defpackage :octopus
  (:use :cl :clws
        :cl-json
        :hu.dwim.defclass-star
        :pal
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
  (:shadowing-import-from :pal :v :message :rotate :random-elt :clamp)
  (:shadowing-import-from :closer-mop :defgeneric
                          :standard-generic-function
                          :defmethod))
