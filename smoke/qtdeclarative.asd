
(in-package #:cl-user) 
(asdf/parse-defsystem:defsystem #:qtdeclarative
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
  "ASDF System wrapper around the qtdeclarative smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QTDECLARATIVE"
  :library-files
  ("QtDeclarative")
  :depends-on
  (:qtcore :qtgui :qtnetwork :qtscript)) 