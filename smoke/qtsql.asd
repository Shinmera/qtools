
 
(asdf/parse-defsystem:defsystem #:qtsql
  :defsystem-depends-on
  (:qtools)
  :class
  "qtools::smoke-module-system"
  :version
  "1.0.0"
  :license
  "Artistic"
  :author
  "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer
  "Nicolas Hafner <shinmera@tymoon.eu>"
  :description
  "ASDF System wrapper around the qtsql smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QTSQL"
  :library-files
  ("QtSql")
  :dependencies
  (t (:qtcore :qtgui))) 
