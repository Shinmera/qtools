
(in-package #:cl-user) 
(asdf/parse-defsystem:defsystem #:sci
  :defsystem-depends-on
  (:qtools)
  :class
  "qtools::smoke-module-system"
  :module
  "SCI"
  :version
  "1.0.0"
  :license
  "Artistic"
  :author
  "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer
  "Nicolas Hafner <shinmera@tymoon.eu>"
  :description
  "ASDF System wrapper around the sci smoke module. Ensures that it is present during compilation and loading of a system.") 