
 
(asdf/parse-defsystem:defsystem #:phonon
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
  "ASDF System wrapper around the phonon smoke module. Ensures that it is present during compilation and loading of a system."
  :module
  "PHONON"
  :library-files
  ("phonon")
  :dependencies
  (:win (:qtcore :qtgui) :lin (:qtcore :qtgui :qtdbus :qtxml) :mac
   (:qtcore :qtgui :qtdbus :qtxml))) 
