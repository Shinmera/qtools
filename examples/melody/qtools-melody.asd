#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qtools-melody
  :name "Qtools-Melody"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple music player using Qt and Phonon."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "package")
               (:file "track-list")
               (:file "melody"))
  :depends-on (:qtools :qtcore :qtgui :phonon))
