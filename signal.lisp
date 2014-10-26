#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun generic-signal (object function &rest args)
  (apply #'emit-signal
         object
         (determined-type-method-name function args)
         (mapcar #'(lambda (a) (if (consp a) (car a) a)) args)))

(defun maybe-unwrap-quote (thing)
  (if (and (listp thing)
           (eql 'quote (first thing)))
      (second thing)
      thing))

(define-compiler-macro generic-signal (&environment env object function &rest args)
  (let* ((all-constant T)
         (function (if (constantp function env)
                       (to-method-name (maybe-unwrap-quote function))
                       (progn (setf all-constant NIL) function)))
         (args (loop for arg in args
                     collect (if (constantp arg env)
                                 (cons arg (qt-type-of arg))
                                 (progn (setf all-constant NIL) arg)))))
    (if all-constant
        `(emit-signal ,object
                      ,(determined-type-method-name function args)
                      ,@(mapcar #'car args))
        `(generic-signal ,object ,function ,@args))))

(defmacro define-signal-method (name args)
  (let ((argvars (loop repeat (length args) collect (gensym "ARG")))
        (args (if (listp (first args)) args (mapcar #'list args)))
        (widget (gensym "WIDGET")))
    `(defgeneric ,name (,widget ,@argvars)
       ,@(apply #'map 'list #'(lambda (&rest args)
                                (let ((method-args (loop for arg in args
                                                         for var in argvars
                                                         for type = arg
                                                         collect `(,var ,type))))
                                  `(:method (,widget ,@method-args)
                                     (emit-signal ,widget ,(specified-type-method-name name args)
                                                  ,@argvars))))
                args))))
