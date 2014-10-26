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

(define-widget-class-option :signal (class name args)
  `(:signals ,(mapcar #'list (enumerate-method-descriptors (to-method-name name) args))))

(define-widget-class-option :slot (class &rest body)
  (form-fiddle:with-destructured-lambda-form
      (:name name :lambda-list args :docstring doc :declarations decls :forms forms) (cons :slot body)
    (let* ((clean-decls (remove 'connected decls :key #'caadr :test #'eql))
           (connections (remove 'connected decls :key #'caadr :test-not #'eql))
           (clean-args (mapcar #'(lambda (a) (if (listp a) (car a) a)) args))
           (this (first clean-args)))
      (setf name (to-method-name name))

      (when connections
        (add-initializer
         class *slot-init-priority*
         `(lambda (,this)
            (with-slots-bound (,this ,class)
              ,@(loop for connection in connections
                      for (object func) = (cdadr connection)
                      collect `(connect! ,object ,func ,this (,name ,@(cdr func))))))))

      `(:slots ,(mapcar #'(lambda (func)
                            `(,func
                              (lambda ,clean-args
                                ,@(when doc (list doc)) ,@clean-decls
                                (with-slots-bound (,(first clean-args) ,class)
                                  ,@forms))))
                        (enumerate-method-descriptors name (mapcar #'cdr (cdr args))))))))

(define-widget-class-option :overrides (class name args &rest body)
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
