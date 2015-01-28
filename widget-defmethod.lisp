#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *method*)
(defvar *method-declarations* (make-hash-table :test 'eql))

(setf (documentation '*method* 'variable)
      "Contains the whole DEFMETHOD form that is currently being processed.")

(defun method-declaration (name)
  "Returns a function to process the method declaration NAME, if one exists.

See (SETF QTOOLS:METHOD-DECLARATION)."
  (gethash name *method-declarations*))

(defun (setf method-declaration) (function name)
  "Sets the FUNCTION to be used to process method declarations of NAME.
The arguments of the function should parse the inner of the declaration.
E.g: (declare (foo bar baz)) could be captured by (a &optional b) with
A=>BAR, B=>BAZ. During evaluation of the function, the special variable
*METHOD* will be bound.

See QTOOLS:*METHOD*."
  (setf (gethash name *method-declarations*) function))

(defun remove-method-declaration (name)
  "Remove the method declaration processor function of NAME."
  (remhash name *method-declarations*))

(defmacro define-method-declaration (name args &body body)
  "Define a new method declaration function of NAME.

See (SETF QTOOLS:METHOD-DECLARATION)."
  `(setf (method-declaration ',name)
         #'(lambda ,args ,@body)))

(defmacro defmethod (&whole whole name &rest args)
  "Defines a new method.

This is identical to CL:DEFMETHOD with one exception:
The only difference is that declarations are scanned and
potentially specially processed. If a declaration is
recognised through METHOD-DECLARATION, it is taken out of
the method definition. The declaration processor function
then may or may not cause side-effects or spit out
additional forms to be output alongside the CL:DEFMETHOD
form.

See CL:DEFMETHOD.
See QTOOLS:METHOD-DECLARATION."
  (declare (ignore name args))
  (destructuring-bind (function name qualifiers lambda-list docstring declarations forms) (form-fiddle:split-lambda-form whole)
    (declare (ignore function))
    (let ((declaration-forms)
          (unknown-declarations)
          (*method* whole))
      (loop for declaration in declarations
            for (name . args) = (second declaration)
            for declaration-function = (method-declaration name)
            do (if declaration-function
                   (push (apply declaration-function args) declaration-forms)
                   (push declaration unknown-declarations)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           ,@declaration-forms)
         (cl:defmethod ,name ,@qualifiers ,lambda-list
           ,@(when docstring (list docstring))
           ,@unknown-declarations
           ,@forms)))))

(define-method-declaration slot (name args)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    (let ((slot (qtools:specified-type-method-name name args)))
      `(set-widget-class-option ',(second (first lambda)) :slots '(,slot ,method)))))

(define-method-declaration override (&optional name)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    (let ((slot (qtools:to-method-name (or name method))))
      `(set-widget-class-option ',(second (first lambda)) :override '(,slot ,name)))))

(define-method-declaration initializer (priority)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    `(set-widget-class-option ',(second (first lambda)) :initializers '(,method ,priority ,method))))

(define-method-declaration finalizer (priority)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    `(set-widget-class-option ',(second (first lambda)) :finalizers '(,method ,priority ,method))))
