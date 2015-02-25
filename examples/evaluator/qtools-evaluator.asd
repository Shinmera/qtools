#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-evaluator
  :name "Qtools-Evaluator"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple REPL in Qt."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "evaluator"))
  :depends-on (:qtools :qtcore :qtgui
               :cl-ppcre
               :trivial-gray-streams))
