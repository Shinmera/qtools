#|
This file is a part of Qtools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *widget-init-priority* 10)
(defvar *slot-init-priority* 20)
(defvar *layout-init-priority* 30)

(define-widget-class-option :defsignals (class name args &rest decls)
  (destructuring-bind (declarations methods) (split decls '(method) :key #'caadr :test #'string=)
    (declare (ignore declarations))
    (when methods
      (assert (= 1 (length methods)) () "Only one method declaration is allowed.")
      (let ((method-name (or (cadadr (first methods))
                             (intern (format NIL "SIGNAL-~a" name)))))
        (with-compile-and-run
          `(define-signal-method (,name ,method-name) ,args)))))
  `(:signals ,(mapcar #'list (enumerate-method-descriptors (to-method-name name) args))))

(define-widget-class-option :defslots (class &rest body)
  ;; Split apart our lambda
  (form-fiddle:with-destructured-lambda-form (:name name :lambda-list args :docstring doc :declarations decls :forms forms) (cons :slot body)
    ;; Filter declarations
    (destructuring-bind (declarations connections methods) (split decls '(connected method) :key #'caadr :test #'string=)
      ;; Build pure forms
      (let* ((clean-args (mapcar #'(lambda (a) (if (listp a) (car a) a)) args))
             (this (first clean-args))
             (cpp-name (to-method-name name))
             (cpp-args (mapcar #'cdr (cdr args)))
             (func-body `(,@(when doc (list doc))
                          ,@declarations
                          (with-slots-bound (,(first clean-args) ,class)
                            ,@forms))))
        ;; Add our connectors
        (when connections
          (add-initializer
           class *slot-init-priority*
           `(lambda (,this)
              (with-slots-bound (,this ,class)
                ,@(loop for connection in connections
                        for (object func) = (cdadr connection)
                        collect `(connect! ,object ,func ,this (,cpp-name ,@(cdr func))))))))
        ;; Define our methods
        (when methods
          (assert (= 1 (length methods)) () "Only one method declaration is allowed.")
          ;; Exchange name of method if specified
          (setf name (or (cadadr (first methods)) name))
          ;; Compile the methods now.
          (flet ((compile-method (&rest types)
                   (with-compile-and-run
                     `(defmethod ,name ((,this ,(class-name class))
                                        ,@(loop for type in types
                                                for var in (cdr clean-args)
                                                collect `(,var ,(ecl-type-for type))))
                        ,@func-body))))
            (apply #'map NIL #'compile-method cpp-args)))
        ;; Sputter out the slots
        (flet ((build-slot (func)
                 `(,func ,(if methods name `(lambda ,clean-args ,@func-body)))))
          `(:defslots ,(mapcar #'build-slot (enumerate-method-descriptors cpp-name cpp-args))))))))

(define-widget-class-option :defoverrides (class name args &rest body)
  `(:override
    ((,(to-method-name name)
      (lambda ,args
        (with-slots-bound (,(first args) ,class)
          ,@body))))))

(define-widget-slot-option :subwidget (class name constructor &rest body)
  `(:direct-slots ((:name ,name :readers () :writers () :initargs () :finalized T))
    :subwidget ((,name ,constructor ,@body))))

(define-widget-class-option :subwidget (class name constructor &rest body)
  (add-initializer
   class *widget-init-priority*
   `(lambda (this)
      (with-slots-bound (this ,class)
        (setf ,name ,constructor)
        ,@body)))
  NIL)

(define-widget-class-option :layout (class name constructor &rest body)
  (add-initializer
   class *layout-init-priority*
   `(lambda (this)
      (with-slots-bound (this ,class)
        (let ((,name ,constructor))
          ,@body
          (#_setLayout this ,name)))))
  NIL)

(define-widget-class-option :initializer (class widget priority &rest body)
  (add-initializer
   class priority
   `(lambda (,widget)
      (with-slots-bound (,widget ,class)
        ,@body)))
  NIL)
