#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *environment-forms* (make-hash-table :test 'equal)
  "Table mapping form names to evaluator functions.")

(defun environment-form (name)
  "Returns a function to process the form of NAME with, if any.."
  (gethash (string name) *environment-forms*))

(defun (setf environment-form) (function name)
  "Sets a new evaluator function for the given form NAME."
  (setf (gethash (string name) *environment-forms*)
        function))

(defun remove-environment-form (name)
  "Removes the environment-form processor named by NAME."
  (remhash (string name) *environment-forms*))

(defmacro define-environment-form (name structure &body body)
  "Define processing for an environment form of NAME with a body of STRUCTURE."
  (let ((form (gensym "FORM"))
        (doc (form-fiddle:lambda-docstring `(() ,@body))))
    `(setf (environment-form ',name)
           #'(lambda (,form)
               ,@(when doc (list doc))
               (destructuring-bind ,structure (cdr ,form)
                 ,@body)))))

(defun list-environment-forms ()
  "Lists all specially handled forms in a widget-environment."
  (loop for key being the hash-keys of *environment-forms*
        collect key))

(defun describe-environment-form (form)
  "Prints out documentation relating to the environment FORM, if any."
  (let ((func (environment-form form)))
    (format T "~a: ~:[No special handling, will evaluate to the same.~;~:[No documentation available.~;~:*~a~]~]"
            form func (when func (documentation func T)))))

(defmacro define-environment-form-class-option (name option)
  "Shorthand macro to translate forms of NAME to widget class OPTION."
  (assert (keywordp option) () "Option must be a keyword!")
  `(define-environment-form ,name (&rest body)
     ,(format NIL "Translates to the class option ~s" option)
     (values
      NIL
      (list (list ,option body)))))

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
        for (forms options) = (multiple-value-list (funcall (or (environment-form (car form))
                                                                #'identity) form))
        when forms
        collect forms into all-forms
        append options into all-options
        finally (return
                  (let ((classdef (find 'define-widget all-forms :key #'car)))
                    (when (and all-options (not classdef))
                      (warn "Class body forms found but no class definition!"))
                    `(progn
                       ,@(when classdef (list (append classdef all-options)))
                       ,@(remove 'define-widget all-forms :key #'car))))))
