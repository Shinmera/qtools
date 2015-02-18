#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defparameter *methods* (make-hash-table :test 'equal))
(defparameter *static-methods* (make-hash-table :test 'equal))
(defparameter *enums* (make-hash-table :test 'equal))
(defparameter *constructors* (make-hash-table :test 'equal))

(defun write-qclass-name (qclass stream)
  (loop for char across (qclass-name qclass)
        do (write-char (char-upcase char) stream)))

(defun write-qmethod-name (qmethod stream)
  (loop with prev-cap = NIL
        for char across (qmethod-name qmethod)
        do (cond ((find char "~#$?"))
                 ((find char "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                  (unless prev-cap
                    (write-char #\- stream))
                  (setf prev-cap T)
                  (write-char char stream))
                 (T (setf prev-cap NIL)
                    (write-char (char-upcase char) stream)))))

(defun cl-constructor-name (method)
  (with-output-to-string (stream)
    (write-string "MAKE-" stream)
    (write-qclass-name (qt::qmethod-class method) stream)))

(defun cl-enum-name (method)
  (with-output-to-string (stream)
    (write-char #\+ stream)
    (write-qclass-name (qt::qmethod-class method) stream)
    (write-qmethod-name method stream)
    (write-char #\+ stream)))

(defun cl-static-method-name (method)
  (with-output-to-string (stream)
    (write-qclass-name (qt::qmethod-class method) stream)
    (write-qmethod-name method stream)))

(defun cl-method-name (method)
  (with-output-to-string (stream)
    (write-qmethod-name method stream)))

(defun clean-method-name (method)
  (string-trim "~#$?" (etypecase method
                        (integer (qmethod-name method))
                        (string method))))

(defun place-for-method (method)
  (cond ((qt::qmethod-enum-p method) *enums*)
        ((or (qt::qmethod-ctor-p method)
             (qt::qmethod-copyctor-p method)) *constructors*)
        ((qt::qmethod-dtor-p method) NIL)
        ((qt::qmethod-internal-p method) NIL)
        ((qt::qmethod-static-p method) *static-methods*)
        (T *methods*)))

(defun process-method (method)
  (let ((name (clean-method-name method))
        (place (place-for-method method)))
    (when place (push method (gethash name place)))))

(defun process-all-methods ()
  (qt::map-methods #'process-method))

(defun %method-see (stream method &rest rest)
  (declare (ignore rest))
  (format stream "~a::~a(~{~a~^, ~})"
          (qclass-name (qt::qmethod-class method))
          (qmethod-name method)
          (mapcar #'qt::qtype-name (qt::list-qmethod-argument-types method))))

(defun generate-docstring (methods)
  (format NIL "Call to Qt method ~a

~{See ~/foo::%method-see/~^~%~}"
          (clean-method-name (first methods))
          (sort (copy-list methods) #'< :key #'qt::qmethod-argument-number)))

(defmacro with-args ((required optional optional-p) methods &body body)
  (let ((argnums (gensym "ARGNUMS"))
        (maxargs (gensym "MAXARGS"))
        (minargs (gensym "MINARGS"))
        (i (gensym "I")))
    `(let* ((,argnums (mapcar #'qt::qmethod-argument-number ,methods))
            (,maxargs (apply #'max ,argnums))
            (,minargs (apply #'min ,argnums))
            (,required (loop for ,i from 0 to ,minargs
                             collect (gensym (format NIL "REQ-~d" ,i))))
            (,optional (loop for ,i from 0 to (- ,maxargs ,minargs)
                             collect (gensym (format NIL "OPT-~d" ,i))))
            (,optional-p (loop for ,i in ,optional
                               collect `(,,i NIL ,(make-symbol (format NIL "~a-P" ,i))))))
       ,@body)))

(defun compile-method (methods)
  (let ((method (clean-method-name (first methods)))
        (name (intern (cl-method-name (first methods))))
        (whole (gensym "WHOLE"))
        (instance (gensym "INSTANCE")))
    (with-args (reqargs optargs optargs-p) methods
      `(progn
         (export ',name)
         (defun ,name (,instance ,@reqargs ,@(when optargs `(&optional ,@optargs-p)))
           ,(generate-docstring methods)
           (cond
             ,@(loop for (arg noop arg-p) in (reverse optargs-p)
                     for count from 0
                     collect `(,arg-p
                               (optimized-call T ,instance ,method ,@reqargs ,@(butlast optargs count))))
             (T (optimized-call T ,instance ,method ,@reqargs))))
         (define-compiler-macro ,name (&whole ,whole ,instance ,@reqargs ,@(when optargs `(&optional ,@optargs)))
           (declare (ignore ,@reqargs ,@optargs))
           `(optimized-call T ,,instance ,,method ,(cddr ,whole)))))))

(defun map-compile-all-methods (function &optional (table *methods*))
  (maphash (lambda (name methods)
             (declare (ignore name))
             (funcall function (compile-method methods)))
           table))

(defun compile-static-method (class methods)
  (let ((method (clean-method-name (qmethod-name (first methods))))
        (name (intern (cl-static-method-name (first methods))))
        (class-name (qclass-name class))
        (whole (gensym "WHOLE")))
    (with-args (reqargs optargs optargs-p) methods
      `(progn
         (export ',name)
         (defun ,name (,@reqargs ,@(when optargs `(&optional ,@optargs-p)))
           ,(generate-docstring methods)
           (cond
             ,@(loop for (arg noop arg-p) in (reverse optargs-p)
                     for count from 0
                     collect `(,arg-p
                               (optimized-call T ,class-name ,method ,@reqargs ,@(butlast optargs count))))
             (T (optimized-call T ,class-name ,method ,@reqargs))))
         (define-compiler-macro ,name (&whole ,whole ,@reqargs ,@(when optargs `(&optional ,@optargs)))
           (declare (ignore ,@reqargs ,@optargs))
           `(optimized-call T ,,class-name ,,method ,(cdr ,whole)))))))

(defun map-compile-static-methods (function methods)
  (let ((bundle (make-hash-table :test 'eq)))
    (dolist (method methods)
      (push method (gethash (qt::qmethod-class method) bundle)))
    (loop for class being the hash-keys of bundle
          for methods being the hash-values of bundle
          do (funcall function (compile-static-method class methods)))))

(defun map-compile-all-static-methods (function &optional (table *static-methods*))
  (maphash (lambda (name methods)
             (declare (ignore name))
             (map-compile-static-methods function methods))
           table))

(defun compile-constructor (methods)
  (let ((name (intern (cl-constructor-name (first methods))))
        (class (qclass-name (qt::qmethod-class (first methods))))
        (whole (gensym "WHOLE")))
    (with-args (reqargs optargs optargs-p) methods
      `(progn
         (export ',name)
         (defun ,name (,@reqargs ,@(when optargs `(&optional ,@optargs-p)))
           ,(generate-docstring methods)
           (cond
             ,@(loop for (arg noop arg-p) in (reverse optargs-p)
                     for count from 0
                     collect `(,arg-p
                               (optimized-new ,class ,@reqargs ,@(butlast optargs count))))
             (T (optimized-new ,class ,@reqargs))))
         (define-compiler-macro ,name (&whole ,whole ,@reqargs ,@(when optargs `(&optional ,@optargs)))
           (declare (ignore ,@reqargs ,@optargs))
           `(optimized-new ,,class ,(cdr ,whole)))))))

(defun map-compile-all-constructors (function &optional (table *constructors*))
  (maphash (lambda (name methods)
             (declare (ignore name))
             (funcall function (compile-constructor methods)))
           table))

(defun map-compile-constants (function methods)
  (loop for method in methods
        for class = (qclass-name (qt::qmethod-class method))
        for name = (intern (cl-enum-name method))
        do (funcall
            function
            `(progn
               (export ',name)
               (defconstant ,name
                 (load-time-value
                  (when (find-class ,class)
                    (optimized-call T ,class ,(qmethod-name method)))))))))

(defun map-compile-all-constants (function &optional (table *enums*))
  (maphash (lambda (name methods)
             (declare (ignore name))
             (map-compile-constants function methods))
           table))

(defun map-compile-everything (function)
  (map-compile-all-methods function)
  (map-compile-all-static-methods function)
  (map-compile-all-constructors function)
  (map-compile-all-constants function))

(macrolet ((define-list-for-mapper (name mapper)
             (let ((table (gensym "TABLE"))
                   (t-p (gensym "TABLE-P"))
                   (list (gensym "LIST"))
                   (result (gensym "RESULT")))
               `(defun ,name (&optional (,table NIL ,t-p))
                  (let ((,list ()))
                    (apply
                     #',mapper #'(lambda (,result)
                                   (push ,result ,list))
                     (when ,t-p (list ,table)))
                    ,list)))))
  (define-list-for-mapper list-compile-all-methods map-compile-all-methods)
  (define-list-for-mapper list-compile-all-static-methods map-compile-all-static-methods)
  (define-list-for-mapper list-compile-all-constructors map-compile-all-constructors)
  (define-list-for-mapper list-compile-all-constants map-compile-all-constants))

(defun list-compile-everything ()
  (let ((list ()))
    (map-compile-everything (lambda (form) (push form list)))
    list))

(defun write-forms (mapper stream)
  (let ((i 0))
    (funcall mapper
             (lambda (form)
               (incf i)
               (when (= 0 (mod i 100))
                 (format T "~&; On ~dth form..." i))
               (print form stream)
               (format stream "~%")))
    (format T "~&; ~d forms processed." i)))

(defun write-section (section mapper stream)
  (format T "~&;; Processing ~a" section)
  (format stream "~&~%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (format stream "~&;;;; ~a~%" section)
  (write-forms mapper stream))

(defun write-all-sections (stream)
  (format T "~&;;; Writing all sections")
  (write-section "Methods" #'map-compile-all-methods stream)
  (write-section "Static Methods" #'map-compile-all-static-methods stream)
  (write-section "Constructors" #'map-compile-all-constructors stream)
  (write-section "Constants" #'map-compile-all-constants stream))

(defun write-everything-to-file (pathname &key (package "Q+") (if-exists :supersede) (body-processor #'write-all-sections))
  (let* ((package (cond ((typep package 'package))
                        ((find-package package) (find-package package))
                        (T (make-package package))))
         (*package* package))
    (with-open-file (stream pathname :direction :output :if-exists if-exists)
      (format stream ";;;;; Automatically generated file to map Qt methods and enums to CL functions and constants.~%")
      (format stream ";;;;; See QTOOLS:WRITE-EVERYTHING-TO-FILE~%")
      (print `(defpackage ,(package-name package)) stream)
      (print `(in-package ,(package-name package)) stream)
      (funcall body-processor stream))))

;; FIXME: For constants, all smoke contribs need to be available at load-time...
