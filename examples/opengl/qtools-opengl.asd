#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-opengl
  :name "Qtools-Opengl"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An example combining Qt and OpenGL."
  :homepage "https://Shinmera.github.io/qtools/"
  :bug-tracker "https://github.com/Shinmera/qtools/issues"
  :source-control (:git "https://github.com/Shinmera/qtools.git")
  :serial T
  :components ((:file "opengl"))
  :depends-on (:qtools :qtcore :qtgui :qtopengl
               :cl-opengl))
