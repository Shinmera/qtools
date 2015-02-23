#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-example
  :name "Qtools-Example"
  :version "0.4.2"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An example use project of Qtools"
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "example"))
  :depends-on (:qtools :qtcore :qtgui
               :cl-ppcre))
