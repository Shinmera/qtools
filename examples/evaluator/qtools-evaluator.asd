#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-evaluator
  :name "Qtools-Evaluator"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple REPL in Qt."
  :homepage "https://Shinmera.github.io/qtools/"
  :bug-tracker "https://github.com/Shinmera/qtools/issues"
  :source-control (:git "https://github.com/Shinmera/qtools.git")
  :serial T
  :components ((:file "evaluator"))
  :depends-on (:qtools :qtcore :qtgui
               :cl-ppcre
               :trivial-gray-streams))
