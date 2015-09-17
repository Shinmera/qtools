#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-game
  :name "Qtools-Game"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple game in Qtools."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "package")
               (:file "primitives")
               (:file "chunk")
               (:file "world")
               (:file "editor")
               (:file "keyboard")
               (:file "game"))
  :depends-on (:qtools :qtcore :qtgui :qtopengl
               :closer-mop))
