#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools
  :version "0.7.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A collection of tools to aid in development with CommonQt."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "class-map")
               (:file "name-translation")
               (:file "dispatch")
               (:file "finalizable")
               (:file "gc-finalized")
               (:file "copying")
               (:file "printing")
               (:file "signal")
               (:file "widget")
               (:file "widget-defmethod")
               (:file "widget-convenience")
               (:file "widget-menu")
               (:file "readtable")
               (:file "generate")
               (:file "dynamic")
               (:file "precompile")
               (:file "deploy")
               (:file "documentation"))
  :depends-on (:qt-libs
               :cl-ppcre
               :closer-mop
               :form-fiddle
               :named-readtables
               :trivial-indent
               :trivial-garbage
               :trivial-main-thread
               :documentation-utils))
