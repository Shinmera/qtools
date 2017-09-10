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
         (mapcar #'(lambda (a) (if (listp a) (car a) a)) args)))

(defun process-list-arg (arg)
  (flet ((handle-unquote (thing)
           (typecase thing
             #+sbcl
             (sb-impl::comma (sb-impl::comma-expr thing))
             (T `(quote ,thing)))))
    (case (first arg)
      (quote (second arg))
      #+sbcl
      (sb-int:quasiquote
       (list (handle-unquote (first (second arg)))
             (handle-unquote (second (second arg)))))
      (T arg))))

(define-compiler-macro generic-signal (&environment env object function &rest args)
  (let* ((all-constant T)
         (function (if (constantp function env)
                       (to-method-name (maybe-unwrap-quote function))
                       (progn (setf all-constant NIL) function)))
         (args (loop for arg in args
                     collect (cond
                               ((constantp arg env) (list arg (qt-type-of arg)))
                               ((listp arg) (process-list-arg arg))    
                               (T (setf all-constant NIL) arg)))))
    (if all-constant
        `(emit-signal ,object
                      ,(determined-type-method-name function (mapcar #'(lambda (a) (mapcar #'maybe-unwrap-quote a)) args))
                      ,@(mapcar #'car args))
        `(generic-signal ,object ,function
                         ,@(mapcar #'(lambda (a) (if (listp a) `(list ,a) a)) args)))))

(defmacro signal! (object function &rest args)
  (let ((obj (gensym "OBJECT")))
    `(let ((,obj ,object))
       (flet ((emit-signal (,obj)
                (emit-signal ,obj ,(specified-type-method-name (first function) (rest function)) ,@args)))
         (if (listp ,obj)
             (mapc #'emit-signal ,obj)
             (emit-signal ,obj))))))

(defmacro connect! (origin origin-function target target-function)
  (let ((orig (gensym "ORIGIN")) (targ (gensym "TARGET")))
    `(flet ((connect (origin target)
              (connect origin ,(specified-type-method-name (car origin-function) (cdr origin-function))
                       target ,(specified-type-method-name (car target-function) (cdr target-function)))))
       (dolist (,orig (ensure-list ,origin))
         (dolist (,targ (ensure-list ,target))
           (connect ,orig ,targ))))))

(defmacro disconnect! (origin origin-function target target-function)
  (let ((orig (gensym "ORIGIN")) (targ (gensym "TARGET")))
    `(flet ((disconnect (origin target)
              (disconnect origin ,(specified-type-method-name (car origin-function) (cdr origin-function))
                          target ,(specified-type-method-name (car target-function) (cdr target-function)))))
       (dolist (,orig (ensure-list ,origin))
         (dolist (,targ (ensure-list ,target))
           (disconnect ,orig ,targ))))))

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
