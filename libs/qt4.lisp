#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :qt4
  :class build-system
  :version "4.8.7")

(defmethod origin ((c (eql (asdf:find-system :qt4))))
  NIL)

(defmethod asdf:perform ((op generate-op) (c (eql (asdf:find-system :qt4))))
  NIL)

(defmethod asdf:perform ((op install-op) (c (eql (asdf:find-system :qt4))))
  (test-prerequisite "Qt4.8" "qmake-qt4" "qmake"))

(defmethod asdf:output-files ((op install-op) (c (eql (asdf:find-system :qt4))))
  #+unix (list #p"/usr/lib/"
               #p"/usr/share/qt4/"
               #p"/usr/include/qt4/"
               #p"/usr/bin/"))
