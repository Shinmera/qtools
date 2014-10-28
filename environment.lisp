#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *environment-forms* (make-hash-table :test 'equal))

(defun environment-form (name)
  (or (gethash (string name) *environment-forms*)
      #'identity))

(defun (setf environment-form) (function name)
  (setf (gethash (string name) *environment-forms*)
        function))

(defmacro define-environment-form (name structure &body body)
  (let ((form (gensym "FORM")))
    `(setf (environment-form ',name)
           #'(lambda (,form)
               (destructuring-bind ,structure (cdr ,form)
                 ,@body)))))

(defmacro define-environment-form-class-option (name option)
  (assert (keywordp option) () "Option must be a keyword!")
  `(define-environment-form ,name (&rest body)
     (values
      NIL
      (list (list ,option (list body))))))

(define-environment-form-class-option define-signal :defsignals)
(define-environment-form-class-option define-slot :defslots)
(define-environment-form-class-option define-overrides :defoverrides)
(define-environment-form-class-option define-subwidget :subwidget)
(define-environment-form-class-option define-layout :layout)
(define-environment-form-class-option define-initializer :initializer)

(defmacro with-widget-environment (&body forms)
  (loop for form in forms
        for (forms options) = (multiple-value-list (funcall (environment-form (car form)) form))
        when forms
        collect forms into all-forms
        append options into all-options
        finally (return
                  (let ((classdef (find 'define-qt-widget all-forms :key #'car)))
                    (when (and all-options (not classdef))
                      (warn "Class body forms found but no class definition!"))
                    `(progn
                       ,@(when classdef (list (append classdef all-options)))
                       ,@(remove 'define-qt-widget all-forms :key #'car))))))
