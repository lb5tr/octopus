(defpackage :octopus
  (:use :cl :clws
        :cl-json
        :hu.dwim.defclass-star
        :log5
        :hu.dwim.def
        :alexandria
        :bordeaux-threads
        :anaphora)
  (:export :start-octopus-server))

