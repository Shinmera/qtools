#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(define-finalizable gc-finalized ()
  ((%object :initarg :object :initform (error "OBJECT required.") :reader unbox :finalized T)))

(defmethod print-object ((finalized gc-finalized) stream)
  (print-unreadable-object (finalized stream :type T)
    (format stream "~s" (unbox finalized)))
  finalized)

(defmethod initialize-instance :after ((finalized gc-finalized) &key)
  (let ((object (unbox finalized)))
    (tg:finalize finalized #'(lambda () (finalize object)))))

(declaim (inline make-gc-finalized))
(defun make-gc-finalized (object)
  (make-instance 'gc-finalized :object object))

(defmacro with-gc-finalized (bindings &body body)
  (let ((gensyms (loop for (symbol form) in bindings
                       collect `(,(gensym (string symbol)) (make-gc-finalized ,form)))))
    `(let ,gensyms
       (symbol-macrolet
           ,(loop for (symbol form) in bindings
                  for (gensym oform) in gensyms
                  collect `(,symbol (unbox ,gensym)))
         ,@body))))
