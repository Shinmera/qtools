
(in-package #:cl-user) 
(asdf/parse-defsystem:defsystem #:qtmultimedia
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
  "ASDF System wrapper around the qtmultimedia smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QTMULTIMEDIA"
  :library-files
  ("QtMultimedia")
  :depends-on
  (:qtcore :qtgui)) 