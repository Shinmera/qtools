#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun generic-signal (object function &rest args)
  "Attempts to signal the function FUNCTION on OBJECT by determining the
types according to the run-time types of the values.

This is SLOW as the signal method has to be determined at run-time and it
is DANGEROUS as the type mapping are ambiguous or even unknown for certain
arguments and as such the wrong signal may be called or even one that does
not actually exist. If you want to explicitly specify the type of the
argument, wrap it in a CONS where the CAR is the value and the CDR is a
string for the according Qt type.

A compiler macro will try to statically determine types as best as possible,
so GENERIC-SIGNAL is save to use for static values."
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
  "Attempts to predetermine as much type information for GENERIC-SIGNAL as possible.
If all types can be determined statically, EMIT-SIGNAL is used directly instead."
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
  "Macro for a more lisp-y writing of EMIT-SIGNAL.
Function should be a list of the METHOD-NAME followed by Qt argument types.
The effective method name is computed as per SPECIFIED-TYPE-METHOD-NAME."
  `(emit-signal ,object ,(specified-type-method-name function (mapcar #'second args))
                ,@(mapcar #'first args)))

(defmacro connect! (origin origin-function target target-function)
  "Macro for a more lisp-y writing of CONNECT.
ORIGIN-FUNCTION and TARGET-FUNCTION should both be a list of the METHOD-NAME
followed by Qt argument types. The effective method name is computed as per
SPECIFIED-TYPE-METHOD-NAME."
  `(connect ,origin ,(specified-type-method-name (car origin-function) (cdr origin-function))
            ,target ,(specified-type-method-name (car target-function) (cdr target-function))))

(defun signal-method-for-name (name)
  (intern (string-upcase (format NIL "SIGNAL-~a" name))))

(defmacro define-signal-method (name args)
  "Shorthand to define wrapper methods for the given signal.

NAME ::= signal | (signal method-name)
ARGS ::= ARG*
ARG  ::= qt-type | (qt-type*)

A methods with name NAME are generated that takes arguments the
object to signal and the specified arguments with their according types.
You may either specify a single type on each argument, or lists of
correlating types for each argument. Each type is resolved as per
ECL-TYPE-FOR to a type to use in the method specializers. The signal
method to call is computed as per SPECIFIED-TYPE-METHOD-NAME."
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
