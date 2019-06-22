#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem qtools-titter
  :name "Qtools-Titter"
  :version "1.0.1"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A primitive Twitter client."
  :homepage "https://Shinmera.github.io/qtools/"
  :bug-tracker "https://github.com/Shinmera/qtools/issues"
  :source-control (:git "https://github.com/Shinmera/qtools.git")
  :serial T
  :components ((:file "titter"))
  :depends-on (:qtools :qtcore :qtgui
               :chirp))
