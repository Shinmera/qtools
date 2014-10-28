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

(defmacro signal! (object function &rest args)
  `(emit-signal ,object ,(specified-type-method-name function (mapcar #'second args))
                ,@(mapcar #'first args)))

(defmacro connect! (origin origin-function target target-function)
  `(connect ,origin ,(specified-type-method-name (car origin-function) (cdr origin-function))
            ,target ,(specified-type-method-name (car target-function) (cdr target-function))))

(defun signal-method-for-name (name)
  (intern (string-upcase (format NIL "SIGNAL-~a" name))))

(defmacro define-signal-method (name args)
  (destructuring-bind (method &optional (name (signal-method-for-name method))) (if (listp name) name (list name))
    (let ((argvars (loop repeat (length args) collect (gensym "ARG")))
          (args (if (listp (first args)) args (mapcar #'list args)))
          (widget (gensym "WIDGET")))
      (flet ((generate-method (&rest types)
               (let ((method-args (loop for type in types
                                        for var in argvars
                                        collect `(,var ,(ecl-type-for type)))))
                 `(:method (,widget ,@method-args)
                    (emit-signal ,widget ,(specified-type-method-name method types)
                                 ,@argvars)))))
        `(defgeneric ,name (,widget ,@argvars)
           ,@(apply #'map 'list #'generate-method args))))))
