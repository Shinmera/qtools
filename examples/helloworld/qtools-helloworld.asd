#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-helloworld
  :name "Qtools-HelloWorld"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Your every day hello world."
  :homepage "https://github.com/Shinmera/qtools"
  :components ((:file "helloworld"))
  :defsystem-depends-on (:qtools)
  :depends-on (:qtcore :qtgui)
  :build-operation "qt-program-op"
  :build-pathname "helloworld"
  :entry-point "qtools-helloworld:main")
