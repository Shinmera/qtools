#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

;;;;;
;; File processing

(defun write-forms (stream)
  (let ((i 0))
    (map-compile-all
     (lambda (form)
       (incf i)
       (when (= 0 (mod i 1000))
         (format T "~&; On ~dth form..." i))
       (when form
         (dolist (form (if (eql (car form) 'progn)
                           (cdr form)
                           (list form)))
           (print form stream))
         (format stream "~%"))))
    (format T "~&; ~d forms processed." i)))

(defun write-everything-to-file (pathname &key (package "Q+") (if-exists :supersede))
  (let* ((package (cond ((typep package 'package))
                        ((find-package package) (find-package package))
                        (T (make-package package))))
         (modules (loaded-smoke-modules))
         (*target-package* package)
         (*package* (find-package '#:cl-user)))
    (with-open-file (stream pathname :direction :output :if-exists if-exists)
      (format T "~&;;;; Writing to file ~a" pathname)
      (format stream "~&;;;;; Automatically generated file to map Qt methods and enums to CL functions and constants.")
      (format stream "~&;;;;; See QTOOLS:WRITE-EVERYTHING-TO-FILE")
      (format stream "~&;;;;")
      (format stream "~&;;;; Active smoke modules: ~{~a~^ ~}" modules)
      (print `(in-package #:cl-user) stream)
      (print `(eval-when (:compile-toplevel :load-toplevel :execute)
                (unless (find-package "QTOOLS")
                  (error "Qtools needs to be loaded first!"))
                (dolist (module ',modules)
                  (qt:ensure-smoke module))
                (unless (find-package ,(package-name package))
                  (make-package ,(package-name package)))) stream)
      (write-forms stream)
      pathname)))

(defun q+-compile-and-load (&key modules (file (merge-pathnames "q+.lisp" (uiop:temporary-directory))))
  (when modules
    (qt::reload)
    (load-all-smoke-modules modules))
  (load (compile-file (write-everything-to-file file) :print NIL) :print NIL))


;;;;;
;; ASDF components

(defun load-for-wrapper (c)
  (etypecase (smoke-module c)
    ((or symbol string) (load-all-smoke-modules (list (smoke-module c))))
    (list (load-all-smoke-modules (smoke-module c))))
  T)

(defclass smoke-module-system (asdf:system)
  ((smoke-module :accessor smoke-module :initarg :module :initform NIL)))

(defmethod asdf:perform ((op asdf:compile-op) (c smoke-module-system))
  (load-for-wrapper c))

(defmethod asdf:perform ((op asdf:load-op) (c smoke-module-system))
  (load-for-wrapper c))

(defun compile-smoke-module-system-definition (module)
  `(asdf:defsystem ,(make-symbol (string-upcase module))
     :defsystem-depends-on (:qtools)
     :class "qtools::smoke-module-system"
     :module ,(string-upcase module)))

(defun write-smoke-module-system-file (module)
  (let ((file (asdf:system-relative-pathname :qtools (format NIL "smoke/~(~a~).asd" module))))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (let ((*package* (find-package :cl-user)))
        (print '(in-package #:cl-user) stream)
        (print (compile-smoke-module-system-definition module) stream)))
    file))

(defun write-all-smoke-module-system-files ()
  (dolist (module *smoke-modules*)
    (write-smoke-module-system-file module)))
