#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-opengl
  :name "Qtools-Opengl"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An example combining Qt and OpenGL."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "opengl"))
  :depends-on (:qtools :qtcore :qtgui :qtopengl
               :cl-opengl))
