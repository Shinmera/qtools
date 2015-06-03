#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem qt-libs
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "System to ensure that the necessary Qt libs are available."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:file "qt-libs"))
  :depends-on (:qt-lib-generator
               :cffi))

#+quicklisp (ql-dist:ensure-installed (ql-dist:find-system :qt))

(flet ((symbf (package name)
         (fdefinition (find-symbol (string name) package)))
       ((setf symbf) (function package name)
         (setf (fdefinition (find-symbol (string name) package)) function)))
  (defmethod asdf:perform :after ((op asdf:compile-op) (c (eql (asdf:find-system :qt-libs))))
    (asdf:compile-system :smokegen)
    (asdf:compile-system :smokeqt)
    (asdf:compile-system :libcommonqt)
    (funcall (symbf :qt-libs :ensure-standalone-libs)))

  (defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :qt-libs))))
    (asdf:load-system :qt))

  (defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :qt))))
    ;; Override standard load function and use ours instead.
    (setf (symbf :qt :load-libcommonqt)
          (symbf :qt-libs :load-libcommonqt))))
