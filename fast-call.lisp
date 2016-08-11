#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defmacro with-call-stack (stack args &body body)
  `(cffi:with-foreign-object (,stack '(:union qt::StackItem) ,(length args))
     ,@(loop for (val type) in args
             for i from 0
             collect `(setf (cffi:foreign-slot-value
                             (cffi:mem-aptr ,stack '(:union qt::StackItem) ,i)
                             '(:union qt::StackItem) ',type)
                            ,val))
     ,@body))

(defmacro fast-direct-call (method object stack)
  `(qt::call-class-fun (load-time-value
                        (qt::qclass-trampoline-fun
                         (qt::qmethod-class ,method)))
                       (load-time-value
                        (qt::qmethod-classfn-index ,method))
                       ,object
                       ,stack))

(defun find-fastcall-method (class name &rest argtypes)
  (let ((methods (ensure-methods (ensure-q+-method name))))
    (loop for method in methods
          for args = (mapcar #'qt::qtype-name (qt::list-qmethod-argument-types method))
          for mclass = (qt::qmethod-class method)
          when (and (= mclass (ensure-qclass class))
                    (every (lambda (a b) (eql (translate-type a 'cffi)
                                              (translate-type b 'cffi)))
                           args argtypes))
          return method)))

(defmacro fast-call (method-descriptor object &rest args)
  (destructuring-bind (obj-type method &rest types) method-descriptor
    (let ((obj (gensym "OBJECT"))
          (stack (gensym "STACK")))
      `(let ((,obj (qt::qobject-pointer ,object)))
         (with-call-stack ,stack ((,obj qt::ptr)
                                  ,@(loop for type in types for arg in args
                                          collect (list arg (translate-type type 'stack-item))))
           (fast-direct-call ,(or (apply #'find-fastcall-method obj-type method types)
                                  (error "Couldn't find method for descriptor ~s"
                                         method-descriptor))
                             ,obj
                             ,stack))))))
