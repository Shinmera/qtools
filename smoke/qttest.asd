
(in-package #:cl-user) 
(asdf/parse-defsystem:defsystem #:qttest
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
  "ASDF System wrapper around the qttest smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QTTEST"
  :library-files
  ("QtTest")
  :dependencies
  (t (:qtcore :qtgui))) 