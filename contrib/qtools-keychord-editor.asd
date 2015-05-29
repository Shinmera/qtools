#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-keychord-editor
  :version "0.6.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple keychord editor dialog."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "keychord-editor"))
  :depends-on (:qtools))
