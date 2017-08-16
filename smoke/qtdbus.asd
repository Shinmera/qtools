
 
(asdf/parse-defsystem:defsystem #:qtdbus
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
  "ASDF System wrapper around the qtdbus smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QTDBUS"
  :library-files
  ("QtDBus")
  :dependencies
  (:lin (:qtcore :qtxml) :mac (:qtcore :qtxml))) 
