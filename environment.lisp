#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *environment-forms* (make-hash-table :test 'equal)
  "Table mapping form names to evaluator functions.")

(defun environment-form (name)
  "Returns a function to process the form of NAME with."
  (or (gethash (string name) *environment-forms*)
      #'identity))

(defun (setf environment-form) (function name)
  "Sets a new evaluator function for the given form NAME."
  (setf (gethash (string name) *environment-forms*)
        function))

(defmacro define-environment-form (name structure &body body)
  "Define processing for an environment form of NAME with a body of STRUCTURE."
  (let ((form (gensym "FORM")))
    `(setf (environment-form ',name)
           #'(lambda (,form)
               (destructuring-bind ,structure (cdr ,form)
                 ,@body)))))

(defmacro define-environment-form-class-option (name option)
  "Shorthand macro to translate forms of NAME to widget class OPTION."
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
  "Compile the inner forms in an environment that allows a more lispy definition style.
The main purpose of this macro is to avoid having to press all of your information into
the class definition form."
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
