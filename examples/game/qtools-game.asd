#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-game
  :name "Qtools-Game"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple game in Qtools."
  :homepage "https://Shinmera.github.io/qtools/"
  :bug-tracker "https://github.com/Shinmera/qtools/issues"
  :source-control (:git "https://github.com/Shinmera/qtools.git")
  :serial T
  :components ((:file "package")
               (:file "primitives")
               (:file "chunk")
               (:file "world")
               (:file "editor")
               (:file "keyboard")
               (:file "player")
               (:file "game"))
  :depends-on (:qtools :qtcore :qtgui :qtopengl
               :closer-mop))
