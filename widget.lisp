#|
This file is a part of Qtools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *widget-class-options* (make-hash-table)
  "Map from option name to class option evaluator.")
(defvar *widget-slot-options* (make-hash-table)
  "Map from option name to slot option evaluator.")

(defclass widget-class (finalizable-class qt-class)
  ((direct-initializers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor widget-class-direct-initializers)
   (direct-finalizers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor widget-class-direct-finalizers)
   (initializers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor widget-class-initializers)
   (finalizers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor widget-class-finalizers))
  (:documentation "Metaclass for widgets. Inherits from FINALIZABLE-CLASS and QT-CLASS."))

(defun add-initializer (class priority function)
  "Adds a new initializer FUNCTION to the CLASS with PRIORITY.
Higher priority means later evaluation. The FUNCTION may be either
a function object or a lambda form to be compiled by COMPILE."
  #+:verbose (v:debug :qtools "Adding initializer ~s" function)
  (let ((function (etypecase function
                    (function function)
                    (list (compile NIL function)))))
    (vector-push-extend (cons priority function) (widget-class-direct-initializers class))))

(defun call-initializers (object)
  "Calls all initializers for the class in sequence.
See ENSURE-CLASS
See ADD-INITIALIZER"
  (loop for init across (widget-class-initializers (ensure-class object))
        do (funcall (cdr init) object)))

(defun add-finalizer (class priority function)
  "Adds a new finalizer FUNCTION to the CLASS with PRIORITY.
Higher priority means later evaluation. The FUNCTION may be either
a function object or a lambda form to be compiled by COMPILE."
  #+:verbose (v:debug :qtools "Adding finalizer ~s" function)
  (let ((function (etypecase function
                    (function function)
                    (list (compile NIL function)))))
    (vector-push-extend (cons priority function) (widget-class-direct-finalizers class))))

(defun call-finalizers (object)
  "Calls all finalizers for the class in sequence.
See ENSURE-CLASS
See ADD-FINALIZER"
  (loop for init across (widget-class-finalizers (ensure-class object))
        do (funcall (cdr init) object)))

(defun widget-class-option (option)
  "Returns the class OPTION evaluator, if any."
  (gethash option *widget-class-options*))

(defun (setf widget-class-option) (function option)
  "Sets a new evaluator FUNCTION for the class OPTION."
  (setf (gethash option *widget-class-options*) function))

(defun remove-widget-class-option (option)
  "Removes the class OPTION evaluator."
  (remhash option *widget-class-options*))

(defun process-widget-class-option (class option bodies)
  "Processes the passed class OPTION for CLASS and returns
a new list of plists of options to use in its stead."
  (let ((func (widget-class-option option)))
    (if func
        (loop #+:verbose initially #+:verbose (v:debug :qtools "Processing class-option ~s" option)
              for body in bodies
              collect (apply func class body))
        `((,option ,bodies)))))

(defun list-widget-class-options ()
  "Lists all widget class options."
  (loop for key being the hash-keys of *widget-class-options*
        collect key))

(defmacro define-widget-class-option (option (class &rest body-lambda) &body forms)
  "Defines a new widget class OPTION.

OPTION      --- The name of the option (keyword).
CLASS       --- The current class being initialized.
BODY-LAMBDA --- A lambda-list to destructure the option's body with.

This should return a plist of options to use in place of the parsed option.

Class-options are evaluated before the Qt-class options are taken into effect,
but after the class' slots have been initialized. You should not change the
class' slot definition with class options. Use a slot-option for that instead.

See DEFINE-WIDGET-SLOT-OPTION."
  (assert (keywordp option) () "Option name must be a keyword.")
  `(setf (widget-class-option ,option)
         #'(lambda (,class ,@body-lambda) ,@forms)))

(defun widget-slot-option (option)
  "Returns the slot OPTION evaluator, fi any."
  (gethash option *widget-slot-options*))

(defun (setf widget-slot-option) (function option)
  "Sets a new evaluator FUNCTION for the slot OPTION."
  (setf (gethash option *widget-slot-options*) function))

(defun remove-widget-slot-option (option)
  "Removes the slot OPTION evaluator."
  (remhash option *widget-slot-options*))

(defun process-widget-slot-option (class option bodies)
  "Processes the passed slot OPTION for CLASS and returns
a new list of plists of options to use in its stead."
  (let ((func (widget-slot-option option)))
    (if func
        (loop #+:verbose initially #+:verbose (v:debug :qtools "Processing slot-option ~s" option)
              for body in bodies
              collect (apply func class body))
        `((,option ,bodies)))))

(defun list-widget-slot-options ()
  "Lists all widget slot options."
  (loop for key being the hash-keys of *widget-slot-options*
        collect key))

(defmacro define-widget-slot-option (option (class &rest body-lambda) &body forms)
  "Same as DEFINE-WIDGET-CLASS-OPTION.
The only difference is that slot options are evaluated before class options
and should be the only ones to add new slot definitions to the options.
When slot options are evaluated, none of the class' slots are ready and
it is unspecified if any part of the class (even its name) is set at all.

See DEFINE-WIDGET-CLASS-OPTION."
  (assert (keywordp option) () "Option name must be a keyword.")
  `(setf (widget-slot-option ,option)
         #'(lambda (,class ,@body-lambda) ,@forms)))

(defun describe-widget-option (option)
  "Prints out documentation relating to the widget class OPTION, if any."
  (let ((class (widget-class-option option))
        (slot (widget-slot-option option)))
    (format T "~a: ~:[No special handling.~;~@[~%CLASS effect: ~a~]~@[~*~%~]~@[~%SLOT effect: ~a~]~]"
            (or class slot) class (and class slot) slot)))

;; We need to manually recreate this in order to ensure that
;; we can pass initargs that are not recognised in slots.
;; This suffices for our hack on SBCL.
(defmethod make-instance ((class (eql (find-class 'widget-class))) &rest initargs)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

;; On CCL we need to reach deeper, additionally to the make-instance override.
#+:ccl
(defmethod ccl::class-slot-initargs :around ((class (eql (find-class 'widget-class))))
  (append (list-widget-slot-options)
          (list-widget-class-options)
          (call-next-method)))

;; Make sure the class is ready for our extra crazy hackery.
(defun ensure-class-ready (class args)
  ;; In order to have the class properly set up, we need to call initialize-instance
  ;; once without our class-option processing. After that we need to manually set
  ;; find-class so that the class is readily registered (something that otherwise
  ;; happens after initialize-instance).
  ;;
  ;; The reason why we need to recall initialize-instance is that (at least on CCL)
  ;; the class seems to, for some reason, not be of the required type 'class yet.
  ;;
  ;; Since we only use ANSI functions here I hope that this will work on all impls,
  ;; but since I'm a realist I expect this hackery to either not be sufficient or
  ;; break horribly on other implementations.
  (let ((init-class (apply #'initialize-instance class 'inner-initialize T args)))
    (setf (find-class (or (getf args :name)
                          (class-name (or init-class class))))
          (or init-class class))))

(defun transform-options (class args func)
  (apply #'fuse-plists
         (loop for (option body) on args by #'cddr
               append (funcall func class option body))))

(defmacro with-redefinitions-muffled (&body body)
  `(locally
       (declare #+sbcl(sb-ext:muffle-conditions sb-kernel:redefinition-warning))
     (handler-bind
         (#+sbcl(sb-kernel:redefinition-warning #'muffle-warning))
       ,@body)))

(defun initialize-widget-class (class next args)
  (with-redefinitions-muffled
    (unless (getf args 'inner-initialize)
      (setf args (transform-options class args #'process-widget-slot-option))
      (ensure-class-ready class args)
      (setf (fill-pointer (widget-class-direct-initializers class)) 0)
      (setf (fill-pointer (widget-class-direct-finalizers class)) 0)
      (setf args (transform-options class args #'process-widget-class-option))
      #+:verbose (v:debug :qtools "Final class options: ~s" args)))
  (apply next class args))

(defmethod initialize-instance :around ((class widget-class) &rest args)
  (initialize-widget-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class widget-class) &rest args)
  (initialize-widget-class class #'call-next-method args))

(defun %inherit-slot (class target source)
  (setf (slot-value class target) (make-array 0 :adjustable T :fill-pointer 0))
  (labels ((inherit (inner)
             (loop for func across (slot-value inner source)
                   do (vector-push-extend func (slot-value class target)))
             (loop for super in (c2mop:class-direct-superclasses inner)
                   when (typep super 'widget-class)
                   do (inherit super))))
    (inherit class))
  (setf (slot-value class target)
        (stable-sort (slot-value class target) #'< :key #'car)))

(defun cascade-option-changes (class)
  (%inherit-slot class 'initializers 'direct-initializers)
  (%inherit-slot class 'finalizers 'direct-finalizers)
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (c2mop:subclassp sub-class (find-class 'widget-class))
                  (c2mop:class-finalized-p sub-class))
        do (cascade-option-changes sub-class)))

(defmethod c2mop:finalize-inheritance :after ((class widget-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-option-changes class))

;; Superclass to further handle integration with the
;; widget-class, as well as to provide a means of
;; defining general methods on all widgets.
(defclass widget (finalizable)
  ((args :initarg :args :initform ()))
  (:metaclass widget-class)
  (:qt-superclass "QObject")
  (:documentation "Superclass for widgets. All your widgets should inherit
from this. See DEFINE-WIDGET."))

;; We can't do with an :after method here since then it
;; would be called AFTER the user's :after method as per
;; the standard method combination order, which is too
;; late for our purposes.
(defmethod initialize-instance ((widget widget) &key)
  "Responsible for calling NEW on the widget instance, as well as invoking the INITIALIZERS."
  (when (next-method-p)
    (call-next-method))
  (apply #'interpret-new widget (slot-value widget 'args))
  (call-initializers widget))

;; Finalizer methods might want to operate on the objects
;; of the class, so we need to execute them before the
;; slots are finalized.
(defmethod finalize :before ((widget widget))
  (call-finalizers widget))

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
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     (:metaclass widget-class)
     (:qt-superclass ,(find-qt-class-name qt-class))
     ,@(fuse-alists options)))

(indent:define-indentation define-widget
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

(defmacro make-widget (class c++-init-args &rest args)
  `(make-instance ,class :args (list ,@c++-init-args) ,@args))
