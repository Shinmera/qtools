
(in-package #:cl-user) 
(asdf/parse-defsystem:defsystem #:qtnetwork
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
  "ASDF System wrapper around the qtnetwork smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QTNETWORK"
  :library-files
  ("QtNetwork")
  :dependencies
  (t (:qtcore))) 