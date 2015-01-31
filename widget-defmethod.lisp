#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *function*)
(defvar *function-declarations* (make-hash-table :test 'eql))

(setf (documentation '*function* 'variable)
      "Contains the whole DEFMETHOD form that is currently being processed.
If you modify the contents of this variable, the changes will be reflected
in the outputted method definition form. However, no declaration that is
processed by function-declarations will ever appear in the output.")

(defun function-declaration (name)
  "Returns a function to process the method declaration NAME, if one exists.

See (SETF QTOOLS:FUNCTION-DECLARATION)."
  (gethash name *function-declarations*))

(defun (setf function-declaration) (function name)
  "Sets the FUNCTION to be used to process method declarations of NAME.
The arguments of the function should parse the inner of the declaration.
E.g: (declare (foo bar baz)) could be captured by (a &optional b) with
A=>BAR, B=>BAZ. During evaluation of the function, the special variable
*METHOD* will be bound.

See QTOOLS:*FUNCTION*"
  (setf (gethash name *function-declarations*) function))

(defun remove-function-declaration (name)
  "Remove the method declaration processor function of NAME."
  (remhash name *function-declarations*))

(defmacro define-function-declaration (name args &body body)
  "Define a new method declaration function of NAME.

See (SETF QTOOLS:FUNCTION-DECLARATION)."
  `(setf (function-declaration ',name)
         #'(lambda ,args ,@body)))

(defun process-declaration-form (whole)
  (let ((*function* (copy-list whole))
        (declaration-forms)
        (known-declarations))
    ;; Process declarations
    (loop for declaration in (form-fiddle:lambda-declarations *function*)
          for (name . args) = (second declaration)
          for declaration-function = (function-declaration name)
          do (when declaration-function
               (push (apply declaration-function args) declaration-forms)
               (push declaration known-declarations)))
    ;; Remove the known declarations from the method body
    (loop for declaration in known-declarations
          do (setf *function* (delete declaration *function*)))
    ;; Change symbol
    (setf (first *function*) 'cl:defmethod)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,@declaration-forms)
       ,*function*)))

(defmacro defun (&whole whole name args &body body)
  "Defines a new function.

This is identical to CL:DEFUN with one exception:
The only difference is that declarations are scanned and
potentially specially processed. If a declaration is
recognised through FUNCTION-DECLARATION, it is taken out of
the method definition. The declaration processor function
then may or may not cause side-effects or spit out
additional forms to be output alongside the CL:DEFMETHOD
form.

See CL:DEFUN.
See QTOOLS:FUNCTION-DECLARATION."
  (declare (ignore name args))
  (process-declaration-form whole))

(defmacro defmethod (&whole whole name &rest args)
  "Defines a new method.

See CL:DEFMETHOD.
See QTOOLS:DEFUN."
  (declare (ignore name args))
  (process-declaration-form whole))

(defmacro with-widget-class ((variable &optional (method '*function*)) &body body)
  `(let ((,variable (second (first (form-fiddle:lambda-lambda-list ,method)))))
     (assert (not (null ,variable)) () "Method must have a primary specializer.")
     (assert (not (listp ,variable)) () "Primary specializer cannot be an EQL-specializer.")
     (locally
         ,@body)))

(define-function-declaration slot (name args)
  (form-fiddle:with-destructured-lambda-form (:name method :declarations declarations) *function*
    (let ((slot (qtools:specified-type-method-name name args))
          (connectors (remove 'connected declarations :test-not #'eql :key #'caadr)))
      (with-widget-class (widget-class)
        (dolist (connector connectors)
          (setf *function* (delete connector *function*)))
        `(progn
           (set-widget-class-option ',widget-class :slots '(,slot ,method))
           ,@(when connectors
               `((define-initializer (,widget-class slot-connectors-init)
                   ,@(loop for connector in connectors
                           for (source source-args) = (rest (second connector))
                           collect `(connect! ,source ,source-args ,widget-class (,name ,@args)))))))))))

(define-function-declaration override (&optional name)
  (let ((slot (qtools:to-method-name (or name (form-fiddle:lambda-name *function*)))))
    (with-widget-class (widget-class)
      `(set-widget-class-option ',widget-class :override '(,slot ,name)))))

(define-function-declaration initializer (priority)
  (let ((method (form-fiddle:lambda-name *function*)))
    (with-widget-class (widget-class)
      `(set-widget-class-option ',widget-class :initializers '(,method ,priority ,method)))))

(define-function-declaration finalizer (priority)
  (let ((method (form-fiddle:lambda-name *function*)))
    (with-widget-class (widget-class)
      `(set-widget-class-option ',widget-class :finalizers '(,method ,priority ,method)))))
