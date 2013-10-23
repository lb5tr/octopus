(defpackage :octopus
  (:use :cl :clws
        :cl-json
        :hu.dwim.defclass-star
        :log5
        :hu.dwim.def
        :hu.dwim.perec
        :ironclad
        :alexandria
        :bordeaux-threads
        :anaphora)
  (:export :start-octopus-server)
  (:shadowing-import-from :hu.dwim.perec :set :time :null))

