#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defclass widget-class (finalizable-class qt-class)
  ((direct-options :initform () :accessor widget-class-direct-options)
   (extern-options :initform () :accessor widget-class-extern-options)
   (direct-initializers :initform () :initarg :initializers :accessor widget-class-direct-initializers)
   (direct-finalizers :initform () :initarg :finalizers :accessor widget-class-direct-finalizers)
   (initializers :initform () :accessor widget-class-initializers)
   (finalizers :initform () :accessor widget-class-finalizers))
  (:documentation "Metaclass for widgets storing necessary information.

The metadata stored in this is mostly responsible for two things:
 1) Providing access to a sequence of mutually independent
    initializers and finalizers for convenient setup and cleanup.
 2) Allowing after-the-fact out-of-form changes to the class
    definition, which is necessary to have for a distributed
    definition form syntax as provided by WIDGET-CONVENIENCE macros.
In order to modify the metadata, please look at SET/REMOVE-WIDGET-CLASS-OPTION."))

(setf (documentation 'widget-class-direct-options 'function)
      "Contains all the options passed to RE/INITIALIZE-INSTANCE when
the class is re/initialized directly through a DEFCLASS form.")
(setf (documentation 'widget-class-extern-options 'function)
      "Contains all the options that are added to the class definition
through external forms and thus need to be included and kept separate
from options directly specified in the class definition.")
(setf (documentation 'widget-class-initializers 'function)
      "A sorted list of functions to be called upon initialization.
This list is overwritten completely whenever the class is re/initialized.

See QTOOLS:CALL-INITIALIZERS")
(setf (documentation 'widget-class-finalizers 'function)
      "A sorted list of functions to be called upon finalization.
This list is overwritten completely whenever the class is re/initialized.

See QTOOLS:CALL-FINALIZERS")

(defclass widget (finalizable)
  ()
  (:metaclass widget-class)
  (:qt-superclass "QObject")
  (:documentation "Common superclass for all widgets in order to allow for
general initialization and cleanup forms that are standardised across all
widgets. 

See QTOOLS:DEFINE-WIDGET."))

(defun call-initializers (class)
  "Calls all the initializers specified on CLASS in their proper sequence.

CLASS can be either an instance of a WIDGET-CLASS, a
WIDGET-CLASS itself, or a symbol naming the class."
  (mapc #'(lambda (function) (funcall (third function) class))
        (widget-class-initializers (ensure-class class)))
  class)

(defun call-finalizers (class)
  "Calls all the finalizers specified on CLASS in their proper sequence.

CLASS can be either an instance of a WIDGET-CLASS, a
WIDGET-CLASS itself, or a symbol naming the class."
  (mapc #'(lambda (function) (funcall (third function) class))
        (widget-class-finalizers (ensure-class class)))
  class)

(defmethod initialize-instance ((widget widget) &key)
  (when (next-method-p)
    (call-next-method))
  ;; FIXME: Some constructors require arguments.
  (new widget)
  (call-initializers widget)
  widget)

(defmethod finalize :before ((widget widget))
  (call-finalizers widget))

(defun setup-widget-class (class next-method &rest options &key initializers finalizers (save-direct-options T) &allow-other-keys)
  "This function should not be called directly, but is instead invoked by the appropriate functions
such as INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, and SOFTLY-REDEFINE-WIDGET-CLASS. In brief,
it concerns itself with proper option merging and filtering before passing it on to the CommonQt
and CLOS methods that process them."
  (declare (ignore initializers finalizers))
  (let ((original-options (copy-list options)))
    ;; Append extra options
    (when (slot-boundp class 'extern-options)
      (loop for (name value) on (widget-class-extern-options class) by #'cddr
            do (setf (getf options name) (append (getf options name) value))))
    ;; Delegate
    (remf options :save-direct-options)
    #+:verbose (unless (getf options 'init) (v:debug :qtools.widget "~s Delegating class options: ~s" class options))
    (remf options 'init)
    (apply next-method class options)
    ;; Save directly specified options
    (when save-direct-options
      (setf (widget-class-direct-options class)
            original-options)))
  class)

(defmethod initialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method 'init T options))

(defmethod reinitialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method options))

(defun set-effective-option (class slot direct-values &key (direct-superclasses (c2mop:class-direct-superclasses class)))
  (let ((values (copy-list direct-values)))
    (dolist (superclass direct-superclasses)
      (when (c2mop:subclassp superclass 'widget)
        (dolist (value (slot-value superclass slot))
          (pushnew value values :key #'third))))
    (setf (slot-value class slot)
          (stable-sort values #'> :key #'second))))

(defun cascade-option-changes (class)
  (set-effective-option class 'initializers (widget-class-direct-initializers class))
  (set-effective-option class 'finalizers (widget-class-direct-finalizers class))
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (c2mop:subclassp sub-class 'widget-class)
                  (c2mop:class-finalized-p sub-class))
        do (cascade-option-changes sub-class)))

(defmethod c2mop:finalize-inheritance :after ((class widget-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-option-changes class))

(defun softly-redefine-widget-class (class)
  "Cause a soft redefinition of the given CLASS.

This will in effect cause a call to REINITIALIZE-INSTANCE with the proper
class options added from WIDGET-CLASS-DIRECT-OPTIONS, followed by a
FINALIZE-INHERITANCE call on the class."
  (let ((class (ensure-class class)))
    #+:verbose (v:debug :qtools.widget "Softly redefining widget class ~s" class)
    ;; Press new options into the class definition
    (apply #'reinitialize-instance class :save-direct-options NIL (copy-list (widget-class-direct-options class)))
    ;; CommonQt performs computations on finalisation
    (c2mop:finalize-inheritance class)
    class))

(defun widget-class-option-p (class option value &key (key #'first) (test #'equal))
  "Tests if OPTION VALUE is already present on CLASS.
Returns the full option value if it can be found.

See QTOOLS:SET-WIDGET-CLASS-OPTION"
  (let ((idents (getf (widget-class-extern-options (ensure-class class)) option)))
    (find (funcall key value) idents :key key :test test)))

(defun set-widget-class-option (class option value &key (key #'first) (test #'equal))
  "Sets a CLASS OPTION VALUE.

The value is identified and distinguished within the OPTION list
by TEST on KEY. If a matching list can be found, it is replaced
at the same position. Otherwise it is appended to the end of the
list. The order here is important to preserve load-order.

See QTOOLS:WIDGET-CLASS-EXTERN-OPTIONS.
See QTOOLS:SOFTLY-REDEFINE-WIDGET-CLASS."
  (let* ((identifier (funcall key value))
         (class (ensure-class class))
         (idents (getf (widget-class-extern-options class) option)))
    (cond ((not idents)
           (setf (getf (widget-class-extern-options class) option)
                 (list value)))
          ((find identifier idents :key key :test test)
           (setf (nth (position identifier idents :key key :test test) idents)
                 value))
          (T
           (setf (cdr (last idents))
                 (list value))))
    (softly-redefine-widget-class class)))

(defun remove-widget-class-option (class option identifier &key (key #'first) (test #'equal))
  "Removes a CLASS OPTION value.

The value is identified and distinguished within the OPTION list
by TEST on KEY. If the first item in the sub-list is EQUAL to IDENTIFIER,
it is removed. This causes a call to SOFTLY-REDEFINE-WIDGET-CLASS.

See QTOOLS:WIDGET-CLASS-EXTERN-OPTIONS.
See QTOOLS:SOFTLY-REDEFINE-WIDGET-CLASS."
  (let ((class (ensure-class class)))
    (setf (getf (widget-class-extern-options class) option)
          (remove identifier (getf (widget-class-extern-options class) option)
                  :key key :test test))
    (softly-redefine-widget-class class)))

(defmacro define-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  "Shorthand over DEFCLASS.

Adds WIDGET as direct-superclass if it does not appear as a
superclass to the specified direct-superclasses. Sets 
WIDGET-CLASS as metaclass and qt-class as the qt-superclass 
after resolving it through FIND-QT-CLASS-NAME.

All options are fused as per FUSE-ALISTS. You may therefore use
the same form multiple times."
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'widget)))
    (push 'widget direct-superclasses))
  (pushnew `(:metaclass widget-class) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  (pushnew `(:qt-superclass ,(eqt-class-name qt-class)) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(indent:define-indentation define-widget
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

;; Widgets are not qobject instances, but they often exhibit
;; a value method, so provide a default here.
(defmethod value ((widget widget))
  (#_value widget))

(defmethod (setf value) (value (widget widget))
  (#_setValue widget value))
