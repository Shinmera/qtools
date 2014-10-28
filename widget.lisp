#|
This file is a part of Qtools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *widget-class-options* (make-hash-table))
(defvar *widget-slot-options* (make-hash-table))

(defclass widget-class (finalizable-class qt-class)
  ((initializers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor qt-widget-initializers)))

(defun add-initializer (class priority function)
  #+:verbose (v:debug :qtools "Adding initializer (~a ~a): ~a" class priority function)
  (let ((function (etypecase function
                    (function function)
                    (list (compile NIL function)))))
    (vector-push-extend (cons priority function) (qt-widget-initializers class))
    (setf (qt-widget-initializers class)
          (sort (qt-widget-initializers class) #'> :key #'car))))

(defun call-initializers (object)
  (loop for init across (qt-widget-initializers (ensure-class object))
        do (funcall (cdr init) object)))

(defun widget-class-option (option)
  (gethash option *widget-class-options*))

(defun (setf widget-class-option) (function option)
  (setf (gethash option *widget-class-options*) function))

(defun process-widget-class-option (class option)
  (let ((func (widget-class-option (first option))))
    (if func
        (loop for body in (cdr option)
              collect (apply func class body))
        (list option))))

(defun list-widget-class-options ()
  (loop for key being the hash-keys of *widget-class-options*
        collect key))

(defmacro define-widget-class-option (option (class &rest body-lambda) &body forms)
  (assert (keywordp option) () "Option name must be a keyword.")
  `(setf (widget-class-option ,option)
         #'(lambda (,class ,@body-lambda) ,@forms)))

(defun widget-slot-option (option)
  (gethash option *widget-slot-options*))

(defun (setf widget-slot-option) (function option)
  (setf (gethash option *widget-slot-options*) function))

(defun process-widget-slot-option (class option)
  (let ((func (widget-slot-option (first option))))
    (if func
        (loop for body in (cdr option)
              collect (apply func class body))
        (list option))))

(defun list-widget-slot-options ()
  (loop for key being the hash-keys of *widget-slot-options*
        collect key))

(defmacro define-widget-slot-option (option (class &rest body-lambda) &body forms)
  (assert (keywordp option) () "Option name must be a keyword.")
  `(setf (widget-slot-option ,option)
         #'(lambda (,class ,@body-lambda) ,@forms)))

(defun describe-widget-option (option)
  (let ((class (widget-class-option option))
        (slot (widget-slot-option option)))
    (format T "~a: ~:[No special handling.~;~@[~%CLASS effect: ~a~]~@[~*~%~]~@[~%SLOT effect: ~a~]~]"
            (or class slot) class (and class slot) slot)))

;; We need to manually recreate this in order to ensure that
;; we can pass initargs that are not recognised in slots.
(defmethod make-instance ((class (eql (find-class 'widget-class))) &rest initargs)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun initialize-widget-class (class next args)
  (let ((args (apply #'fuse-plists
                     (loop for (option body) on args by #'cddr
                           append (process-widget-slot-option class `(,option ,body))))))
    (apply #'shared-initialize class T args)
    (setf (qt-widget-initializers class) (make-array 0 :adjustable T :fill-pointer 0))
    (let ((args (apply #'fuse-plists
                       (loop for (option body) on args by #'cddr
                             append (process-widget-class-option class `(,option ,body))))))
      (apply next class args))))

(defmethod initialize-instance :around ((class widget-class) &rest args)
  (initialize-widget-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class widget-class) &rest args)
  (initialize-widget-class class #'call-next-method args))

;; Superclass to further handle integration with the
;; widget-class, as well as to provide a means of
;; defining general methods on all widgets.
(defclass widget (finalizable)
  ()
  (:metaclass widget-class))

;; We can't do with an :after method here since then it
;; would be called AFTER the user's :after method as per
;; the standard method combination order, which is too
;; late for our purposes.
(defmethod initialize-instance ((widget widget) &key)
  (when (next-method-p)
    (call-next-method))
  (new widget)
  (call-initializers widget))

(defmacro define-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  `(defclass ,name (widget ,@direct-superclasses)
     ,direct-slots
     (:metaclass widget-class)
     (:qt-superclass ,(find-qt-class-name qt-class))
     ,@(fuse-alists options)))

(indent:define-indentation define-widget
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))
