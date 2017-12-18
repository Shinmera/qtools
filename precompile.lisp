#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

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

(defun write-everything-to-file (pathname &key (package *target-package*) (if-exists :supersede))
  (let* ((package (cond ((typep package 'package) package)
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
                (setf qtools:*target-package*
                      (or (find-package ,(package-name package))
                          (make-package ,(package-name package) :use ())))) stream)
      (write-forms stream)
      pathname)))

(defun q+-compile-and-load (&key modules (file (merge-pathnames "q+.lisp" (uiop:temporary-directory))))
  (when modules
    (qt::reload)
    (apply #'load-all-smoke-modules modules))
  (load (compile-file (write-everything-to-file file) :print NIL) :print NIL))
