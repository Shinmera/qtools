#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defun %make-slots-bound-proper (widget-class body)
  (form-fiddle:with-destructured-lambda-form (:docstring doc :declarations declarations :forms forms) `(lambda () ,@body)
    `(,@declarations
      ,@(when doc `(,doc))
      (qtools:with-slots-bound (,widget-class ,widget-class)
        ,@forms))))

(defmacro define-slot ((widget-class slot &optional method-name) args &body body)
  "Define a new SLOT on WIDGET-CLASS with ARGS.

ARGS is a list of arguments, where each item is a list of two values,
the first being the symbol used to bind the value within the function
body, and the second being a type specifier usable for the slot definition
and, if possible, as a specializer in the method. You may specify an
explicit type to use for the method specializer as a third item. If no
explicit type is passed, the Qt type is translated using CL-TYPE-FOR.

In effect this translates to a method definition with METHOD-NAME that
specialises (and binds) on WIDGET-CLASS, with additional required arguments
ARGS, and a SLOT declaration. Additionally, the body is wrapped in a
WITH-SLOTS-BOUND to allow for convenient slot access.

See QTOOLS:CL-TYPE-FOR
See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See CommonQt/slots"
  (setf method-name (or method-name (intern (format NIL "%~a-SLOT-~a" widget-class slot) *package*)))
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class) ,@(loop for arg in args
                                                                        for type = (or (third arg)
                                                                                       (cl-type-for (or (second arg)
                                                                                                        (error "Qt type specifier required for ~s"
                                                                                                               (first arg))))
                                                                                       (emit-compilation-note
                                                                                        "Unable to determine CL-type of ~s for argument ~s, falling back to T."
                                                                                        (second arg) (first arg)))
                                                                        collect `(,(first arg) ,(or type T))))
     (declare (slot ,slot ,(mapcar #'second args)))
     ,@(%make-slots-bound-proper widget-class body)))

(defmacro define-override ((widget-class override &optional method-name) args &body body)
  "Define a new OVERRIDE on WIDGET-CLASS with ARGS.

This is translated to a method definition with METHOD-NAME that specialises
 (and binds) on WIDGET-CLASS, with ARGS appended to the list, and an OVERRIDE
declaration in the body. Additionally, the body is wrapped in a WITH-SLOTS-BOUND
to allow for convenient slot access.

See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See CommonQt/override"
  (setf method-name (or method-name (intern (format NIL "%~a-OVERRIDE-~a" widget-class override) *package*)))
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class) ,@args)
     (declare (override ,override))
     ,@(%make-slots-bound-proper widget-class body)))

(defmacro define-initializer ((widget-class method-name &optional (priority 0)) &body body)
  "Defines a new initializer of METHOD-NAME on WIDGET-CLASS.

Initializers are functions that are run immediately after the widget has been
allocated by QT:NEW, but before any INITIALIZE-INSTANCE:AFTER methods are
executed. They are executed in order of highest PRIORITY first. 

This is translated to a method definition specialised (and bound) on WIDGET-CLASS
with a INITIALIZER declaration. The BODY is wrapped in a WITH-SLOTS-BOUND form.

See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND"
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class))
     (declare (initializer ,priority))
     (when (next-method-p)
       (call-next-method))
     #+:verbose (v:trace :qtools ,(format NIL "Running initializer ~a for ~~a" method-name) ,widget-class)
     ,@(%make-slots-bound-proper widget-class body)))

(defmacro define-finalizer ((widget-class method-name &optional (priority 0)) &body body)
  "Defines a new finalizer of METHOD-NAME on WIDGET-CLASS.

Finalizers are functions that are run immediately after the widget has been
FINALIZED, but before the main FINALIZE method kicks in. This means that the
widget will still be allocated at the time. Finalizers are executed in order 
of highest PRIORITY first. 

This is translated to a method definition specialised (and bound) on WIDGET-CLASS
with a FINALIZER declaration. The BODY is wrapped in a WITH-SLOTS-BOUND form.

See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See QTOOLS:FINALIZE"
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class))
     (declare (finalizer ,priority))
     #+:verbose (v:trace :qtools ,(format NIL "Running finalizer ~a for ~~a" method-name) ,widget-class)
     ,@(%make-slots-bound-proper widget-class body)
     (when (next-method-p)
       (call-next-method))))

(defmacro define-signal ((widget-class signal) args &body options)
  "Define a new SIGNAL on WIDGET-CLASS with ARGS.

This evaluates to a simple SET-WIDGET-CLASS-OPTION that adds a new :SIGNAL
definition to the WIDGET-CLASS. The signal signature is generated using
SPECIFIED-TYPE-METHOD-NAME.

See CommonQt/signals"
  (declare (ignore options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-widget-class-option ',widget-class :signals '(,(qtools:specified-type-method-name signal args)))))

(defun subwidget-initializer-symbol (widget-class name)
  (intern (format NIL "%~a-SUBWIDGET-~a-INITIALIZER" widget-class name) *package*))

(defmacro define-subwidget ((widget-class name) initform &body body)
  "Defines a new sub-widget of NAME on WIDGET-CLASS.

What this means is that a finalized slot of NAME is added to WIDGET-CLASS
as well as an initializer function for the slot. The slot for the sub-widget 
is set to the value returned by the INITFORM, after which BODY is run. BODY
is wrapped in a WITH-SLOTS-BOUND form, so all slots are conveniently available.

See QTOOLS:DEFINE-INITIALIZER"
  (let ((initfunc (subwidget-initializer-symbol widget-class name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (unless (widget-class-option-p ',widget-class :direct-slots '(:name ,name :readers NIL :writers NIL :initargs NIL :finalized T) :key #'identity)
           (set-widget-class-option ',widget-class :direct-slots '(:name ,name :readers NIL :writers NIL :initargs NIL :finalized T) :key #'second)))
       (define-initializer (,widget-class ,initfunc 10)
         (setf ,name ,initform)
         ,@body))))

(defun remove-slot (widget-class slot)
  "Removes the SLOT definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS"
  (remove-widget-class-option (ensure-class widget-class) :slots slot))

(defun remove-override (widget-class override)
  "Removes the OVERRIDE definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the override.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS"
  (remove-widget-class-option (ensure-class widget-class) :override override))

(defun remove-initializer (widget-class initializer)
  "Removes the INITIALIZER definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS"
  (remove-widget-class-option (ensure-class widget-class) :initializers initializer))

(defun remove-finalizer (widget-class finalizer)
  "Removes the FINALIZER definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS"
  (remove-widget-class-option (ensure-class widget-class) :finalizers finalizer))

(defun remove-signal (widget-class signal)
  "Removes the SIGNAL definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS"
  (remove-widget-class-option (ensure-class widget-class) :signals signal))

(defun remove-subwidget (widget-class subwidget)
  "Removes the SUBWIDGET definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the subwidget.
It does however remove the class-slot and initializer of the subwidget.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS"
  (let ((class (ensure-class widget-class)))
    (remove-widget-class-option class :direct-slots subwidget :key #'second)
    (remove-initializer class (subwidget-initializer-symbol (class-name class) subwidget))))
