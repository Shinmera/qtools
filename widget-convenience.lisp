#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defmacro define-slot ((widget-class method-name &optional (slot method-name)) args &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class) ,@(mapcar #'first args))
     (declare (slot ,slot ,(mapcar #'second args)))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-override ((widget-class method-name &optional (override method-name)) args &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class) ,@args)
     (declare (override ,override))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-initializer ((widget-class method-name &optional (priority 0)) &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class))
     (declare (initializer ,priority))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-finalizer ((widget-class method-name &optional (priority 0)) &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class))
     (declare (finalizer ,priority))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-signal ((widget-class signal-name) args &body options)
  (declare (ignore options))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-widget-class-option ',widget-class :signals '(,(qtools:specified-type-method-name signal-name args)))))

(defmacro define-subwidget ((widget-class name) initform &body body)
  (let ((initfunc (intern (format NIL "%INITIALIZE-~a-SUBWIDGET-~a" widget-class name) *package*)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (set-widget-class-option ',widget-class :direct-slots '(:name ,name :readers NIL :writers NIL :initargs NIL :finalized T))
         (set-widget-class-option ',widget-class :initializers '(,name 10 ,initfunc)))
       (defun ,initfunc (,widget-class)
         (setf (slot-value ,widget-class ',name) ,initform)
         (qtools:with-slots-bound (,widget-class ,widget-class)
           ,@body)))))
