
(in-package #:cl-user) 
(asdf/parse-defsystem:defsystem #:qwt
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
  "ASDF System wrapper around the qwt smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QWT"
  :library-files
  ("qwt")
  :dependencies
  (:win (:qtcore :qtgui :qtsvg) :lin (:qtcore :qtgui) :mac (:qtcore :qtgui))) 