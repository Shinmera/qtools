#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-helloworld
  :name "Qtools-HelloWorld"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Your every day hello world."
  :homepage "https://Shinmera.github.io/qtools/"
  :bug-tracker "https://github.com/Shinmera/qtools/issues"
  :source-control (:git "https://github.com/Shinmera/qtools.git")
  :components ((:file "helloworld"))
  :defsystem-depends-on (:qtools)
  :depends-on (:qtcore :qtgui)
  :build-operation "qt-program-op"
  :build-pathname "helloworld"
  :entry-point "qtools-helloworld:main")
