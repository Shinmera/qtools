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
   (initializers :initform () :accessor widget-class-initializers)
   (finalizers :initform () :accessor widget-class-finalizers)))

(defclass widget (finalizable)
  ()
  (:metaclass widget-class)
  (:qt-superclass "QObject"))

(defun call-initializers (class)
  (mapc #'(lambda (function) (funcall function class))
        (widget-class-initializers (ensure-class class)))
  class)

(defun call-finalizers (class)
  (mapc #'(lambda (function) (funcall function class))
        (widget-class-finalizers (ensure-class class)))
  class)

(defmethod initialize-instance ((widget widget) &key)
  (when (next-method-p)
    (call-next-method))
  (new widget)
  (call-initializers widget))

(defmethod finalize :before ((widget widget))
  (call-finalizers widget))

(defun setup-widget-class (class next-method &rest options &key initializers finalizers (save-direct-options T) &allow-other-keys)
  (declare (ignore initializers finalizers))
  ;; Append extra options
  (when (slot-boundp class 'extern-options)
    (loop for (name value) on (widget-class-extern-options class) by #'cddr
          do (setf (getf options name) (append (getf options name) value))))
  (let ((initializers (getf options :initializers))
        (finalizers (getf options :finalizers)))
    ;; Delegate
    (remf options :initializers)
    (remf options :finalizers)
    (remf options :save-direct-options)
    (print options)
    (apply next-method class options)
    ;; Now that the class is ready, process init/finalizers
    (flet ((sort-clean (list)
             (mapcar #'third (stable-sort (copy-list list) #'> :key #'second))))
      (setf (widget-class-initializers class) (sort-clean initializers))
      (setf (widget-class-finalizers class) (sort-clean finalizers)))
    ;; Save directly specified options
    (when save-direct-options
      (setf (widget-class-direct-options class) options))))

(defmethod initialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method options))

(defmethod reinitialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method options))

(defun softly-redefine-widget-class (class)
  (let ((class (ensure-class class)))
    ;; Press new options into the class definition
    (apply #'reinitialize-instance class :save-direct-options NIL (copy-list (widget-class-direct-options class)))
    ;; CommonQt performs computations on finalisation
    (c2mop:finalize-inheritance class)))

(defun set-widget-class-option (class option value)
  (let* ((identifier (car value))
         (class (ensure-class class))
         (idents (getf (widget-class-extern-options class) option)))
    (setf (getf (widget-class-extern-options class) option)
          (cons value (remove identifier idents :key #'first :test #'equal)))
    (softly-redefine-widget-class class)))

(defun remove-widget-class-option (class option identifier)
  (let ((class (ensure-class class)))
    (setf (getf (widget-class-extern-options class) option)
          (remove identifier (getf (widget-class-extern-options class) option)
                  :key #'first :test #'equal))
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

;; Widgets are not qobject instances, but they often exhibit
;; a value method, so provide a default here.
(defmethod value ((widget widget))
  (#_value widget))

(defmethod (setf value) (value (widget widget))
  (#_setValue widget value))
