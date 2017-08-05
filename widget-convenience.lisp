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
  (setf method-name (or method-name (intern (format NIL "%~a-SLOT-~a" widget-class slot) *package*)))
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class) ,@(loop for arg in args
                                                                        for type = (or (third arg)
                                                                                       (translate-name
                                                                                        (or (second arg)
                                                                                            (error "Qt type specifier required for ~s"
                                                                                                   (first arg)))
                                                                                        'class NIL)
                                                                                       (emit-compilation-note
                                                                                        "Unable to determine CL-type of ~s for argument ~s, falling back to T."
                                                                                        (second arg) (first arg)))
                                                                        collect `(,(first arg) ,(or type T))))
     (declare (slot ,slot ,(mapcar #'second args)))
     ,@(%make-slots-bound-proper widget-class body)))

(defmacro define-override ((widget-class override &optional method-name) args &body body)
  (setf method-name (or method-name (intern (format NIL "%~a-OVERRIDE-~a" widget-class override) *package*)))
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class) ,@args)
     (declare (override ,override))
     ,@(%make-slots-bound-proper widget-class body)))

(defmacro define-initializer ((widget-class method-name &optional (priority 0)) &body body)
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class))
     (declare (initializer ,priority))
     (when (next-method-p)
       (call-next-method))
     #+:verbose (v:trace :qtools ,(format NIL "Running initializer ~a for ~~a" method-name) ,widget-class)
     ,@(%make-slots-bound-proper widget-class body)))

(defmacro define-finalizer ((widget-class method-name &optional (priority 0)) &body body)
  `(cl+qt:defmethod ,method-name ((,widget-class ,widget-class))
     (declare (finalizer ,priority))
     #+:verbose (v:trace :qtools ,(format NIL "Running finalizer ~a for ~~a" method-name) ,widget-class)
     ,@(%make-slots-bound-proper widget-class body)
     (when (next-method-p)
       (call-next-method))))

(defmacro define-signal ((widget-class signal) args &body options)
  (declare (ignore options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-widget-class-option ',widget-class :signals '(,(qtools:specified-type-method-name signal args)))))

(defun subwidget-initializer-symbol (widget-class name)
  (intern (format NIL "%~a-SUBWIDGET-~a-INITIALIZER" widget-class name) *package*))

(defmacro define-subwidget ((widget-class name) initform &body body)
  (when (eql widget-class name)
    (cerror "Trust me, I know what I'm doing." "I really doubt you want to name the subwidget the same as the class."))
  (let ((initfunc (subwidget-initializer-symbol widget-class name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (unless (widget-class-option-p ',widget-class :direct-slots '(:name ,name :readers NIL :writers NIL :initargs NIL :finalized T) :key #'identity)
           (set-widget-class-option ',widget-class :direct-slots '(:name ,name :readers NIL :writers NIL :initargs NIL :finalized T) :key #'second)))
       (define-initializer (,widget-class ,initfunc 10)
         (setf ,name ,initform)
         ,@body))))

(trivial-indent:define-indentation define-subwidget (6 4 &body))

(defmacro define-subobject ((object-class name) initform &body body)
  `(define-subwidget (,object-class ,name) ,initform ,@body))

(trivial-indent:define-indentation define-subobject (6 4 &body))

(defun remove-slot (widget-class slot)
  (remove-widget-class-option (ensure-class widget-class) :slots slot)
  (remove-widget-class-option (ensure-class widget-class) :initializers (slot-initializer-symbol slot)))

(defun remove-override (widget-class override)
  (remove-widget-class-option (ensure-class widget-class) :override override))

(defun remove-initializer (widget-class initializer)
  (remove-widget-class-option (ensure-class widget-class) :initializers initializer))

(defun remove-finalizer (widget-class finalizer)
  (remove-widget-class-option (ensure-class widget-class) :finalizers finalizer))

(defun remove-signal (widget-class signal)
  (remove-widget-class-option (ensure-class widget-class) :signals signal))

(defun remove-subwidget (widget-class subwidget)
  (let ((class (ensure-class widget-class)))
    (remove-widget-class-option class :direct-slots subwidget :key #'second)
    (remove-initializer class (subwidget-initializer-symbol (class-name class) subwidget))))
