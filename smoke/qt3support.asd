
 
(asdf/parse-defsystem:defsystem #:qt3support
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
  "ASDF System wrapper around the qt3support smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "QT3SUPPORT"
  :library-files
  ("Qt3Support")
  :dependencies
  (t (:qtcore :qtgui :qtxml :qtnetwork :qtsql))) 
