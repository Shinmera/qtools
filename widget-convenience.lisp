#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defmacro define-slot ((widget-class method-name &optional (slot method-name)) args &body body)
  "Define a new SLOT on WIDGET-CLASS with ARGS.

ARGS is a list of arguments, where each item is a list of two values,
the first being the symbol used to bind the value within the function
body, and the second being a type specifier usable for the slot definition.

In effect this translates to a method definition with METHOD-NAME that
specialises (and binds) on WIDGET-CLASS, with additional required arguments
ARGS, and a SLOT declaration. Additionally, the body is wrapped in a
WITH-SLOTS-BOUND to allow for convenient slot access.

See QTOOLS:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See CommonQt/slots"
  `(defmethod ,method-name ((,widget-class ,widget-class) ,@(mapcar #'first args))
     (declare (slot ,slot ,(mapcar #'second args)))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-override ((widget-class method-name &optional (override method-name)) args &body body)
  "Define a new OVERRIDE on WIDGET-CLASS with ARGS.

This is translated to a method definition with METHOD-NAME that specialises
 (and binds) on WIDGET-CLASS, with ARGS appended to the list, and an OVERRIDE
declaration in the body. Additionally, the body is wrapped in a WITH-SLOTS-BOUND
to allow for convenient slot access.

See QTOOLS:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See CommonQt/override"
  `(defmethod ,method-name ((,widget-class ,widget-class) ,@args)
     (declare (override ,override))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-initializer ((widget-class method-name &optional (priority 0)) &body body)
  "Defines a new initializer of METHOD-NAME on WIDGET-CLASS.

Initializers are functions that are run immediately after the widget has been
allocated by QT:NEW, but before any INITIALIZE-INSTANCE:AFTER methods are
executed. They are executed in order of highest PRIORITY first. 

This is translated to a method definition specialised (and bound) on WIDGET-CLASS
with a INITIALIZER declaration. The BODY is wrapped in a WITH-SLOTS-BOUND form.

See QTOOLS:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND"
  `(defmethod ,method-name ((,widget-class ,widget-class))
     (declare (initializer ,priority))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-finalizer ((widget-class method-name &optional (priority 0)) &body body)
  "Defines a new finalizer of METHOD-NAME on WIDGET-CLASS.

Finalizers are functions that are run immediately after the widget has been
FINALIZED, but before the main FINALIZE method kicks in. This means that the
widget will still be allocated at the time. Finalizers are executed in order 
of highest PRIORITY first. 

This is translated to a method definition specialised (and bound) on WIDGET-CLASS
with a FINALIZER declaration. The BODY is wrapped in a WITH-SLOTS-BOUND form.

See QTOOLS:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See QTOOLS:FINALIZE"
  `(defmethod ,method-name ((,widget-class ,widget-class))
     (declare (finalizer ,priority))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-signal ((widget-class signal) args &body options)
  "Define a new SIGNAL on WIDGET-CLASS with ARGS.

This evaluates to a simple SET-WIDGET-CLASS-OPTION that adds a new :SIGNAL
definition to the WIDGET-CLASS. The signal signature is generated using
SPECIFIED-TYPE-METHOD-NAME.

See CommonQt/signals"
  (declare (ignore options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-widget-class-option ',widget-class :signals '(,(qtools:specified-type-method-name signal args)))))

(defmacro define-subwidget ((widget-class name) initform &body body)
  "Defines a new sub-widget of NAME on WIDGET-CLASS.

What this means is that a finalized slot of NAME is added to WIDGET-CLASS
as well as an initializer function for the slot. The slot for the sub-widget 
is set to the value returned by the INITFORM, after which BODY is run. BODY
is wrapped in a WITH-SLOTS-BOUND form, so all slots are conveniently available.

See QTOOLS:DEFINE-INITIALIZER"
  (let ((initfunc (intern (format NIL "%INITIALIZE-~a-SUBWIDGET-~a" widget-class name) *package*)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (set-widget-class-option ',widget-class :direct-slots '(:name ,name :readers NIL :writers NIL :initargs NIL :finalized T) :key #'second))
       (define-initializer (,widget-class ,initfunc 10)
         (setf (slot-value ,widget-class ',name) ,initform)
         ,@body))))
