#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:org.shirakumo.qtools.q+.asdf
  (:use #:cl))
(in-package #:org.shirakumo.qtools.q+.asdf)

(defclass dynamic-smoke-wrapper (asdf:cl-source-file)
  ())

(defmethod asdf:perform ((op asdf:prepare-op) (c dynamic-smoke-wrapper))
  (flet ((qtools (name &rest args)
           (apply (find-symbol (string name) :qtools) args)))
    (let ((module-buffer (merge-pathnames "q+modules.lisp-expr" (asdf:component-pathname c))))
      (unless (and (uiop:file-exists-p (asdf:component-pathname c))
                   (equal (qtools 'loaded-smoke-modules)
                          (with-open-file (stream module-buffer :if-does-not-exist NIL)
                            (when stream (read stream)))))
        (qtools 'ensure-methods-processed)
        (qtools 'write-everything-to-file (asdf:component-pathname c))
        (with-open-file (stream module-buffer :direction :output :if-exists :supersede)
          (print (qtools 'loaded-smoke-modules) stream))))))

(asdf:defsystem #:q+
  :version "0.2.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Precompiles all Q+ method wrappers for currently active smoke modules."
  :homepage "https://github.com/Shinmera/qtools"
  :serial T
  :components ((:dynamic-smoke-wrapper "q+"))
  :depends-on (:qtools))
