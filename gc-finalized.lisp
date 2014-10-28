#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(define-finalizable gc-finalized ()
  ((%object :initarg :object :initform (error "OBJECT required.") :reader unbox :finalized T))
  (:documentation "Wrapper object to allow automatic calling of FINALIZE by the GC.
Since you cannot finalize the object that is GC-ed itself, we need to wrap our to-
be-finalized object in another object that takes all the references instead.

This means that if you wish your object to remain unfinalized, you need to retain
references to the wrapper. As soon as the wrapper is hit by the GC, FINALIZE is
called on the object it contains.

In order to retrieve the contained object, use UNBOX."))

(defmethod print-object ((finalized gc-finalized) stream)
  (print-unreadable-object (finalized stream :type T)
    (format stream "~s" (unbox finalized)))
  finalized)

(defmethod initialize-instance :after ((finalized gc-finalized) &key)
  (let ((object (unbox finalized)))
    (tg:finalize finalized #'(lambda () (finalize object)))))

(declaim (inline make-gc-finalized))
(defun make-gc-finalized (object)
  "Wrap the OBJECT in a GC-FINALIZED instance. Use UNBOX to retrieve the object again."
  (make-instance 'gc-finalized :object object))

(defmacro with-gc-finalized (bindings &body body)
  "Creates bindings as per LET with the special note that each value of a binding is wrapped
in a GC-FINALIZED. Each bound symbol is shadowed by a SYMBOL-MACROLET, which evaluates to
the bound value as per UNBOX.

In other words, this will look like a standard LET to you, but each value of the let is
automatically ensured to be GC-ed and FINALIZEd once the body exits."
  (let ((gensyms (loop for (symbol form) in bindings
                       collect `(,(gensym (string symbol)) (make-gc-finalized ,form)))))
    `(let ,gensyms
       (symbol-macrolet
           ,(loop for (symbol form) in bindings
                  for (gensym oform) in gensyms
                  collect `(,symbol (unbox ,gensym)))
         ,@body))))
