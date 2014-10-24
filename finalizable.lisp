#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defclass finalizable-class (c2mop:standard-class)
  ())

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class c2mop:standard-class) (superclass finalizable-class))
  T)

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass c2mop:standard-class))
  T)

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass finalizable-class))
  T)

(defclass finalizable-slot ()
  ((finalized :initarg :finalized :initform NIL :reader finalized)))

(defmethod print-object ((slot finalizable-slot) stream)
  (print-unreadable-object (slot stream :type T :identity T)
    (when (finalized slot)
      (write-string "finalized" stream))))

(defclass finalizable-direct-slot-definition (finalizable-slot c2mop:standard-direct-slot-definition)
  ())

(defclass finalizable-effective-slot-definition (finalizable-slot c2mop:standard-effective-slot-definition)
  ())

(defmethod c2mop:direct-slot-definition-class ((class finalizable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'finalizable-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class finalizable-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'finalizable-effective-slot-definition))

(defclass finalizable ()
  ()
  (:metaclass finalizable-class))

(defmacro define-finalizable (name direct-superclasses direct-slots &rest options)
  `(defclass ,name (finalizable ,@direct-superclasses)
     ,direct-slots
     (:metaclass finalizable-class)
     ,@options))

(defgeneric finalize (object)
  #+:verbose
  (:method :before (object)
    (v:trace :qtools "Finalizing ~s" object))
  (:method (object)
    object)
  (:method ((object abstract-qobject))
    (call-next-method)
    (maybe-delete-qobject object)
    object)
  (:method ((object finalizable))
    (call-next-method)
    (loop for slot in (c2mop:class-direct-slots (class-of object))
          for slot-name = (c2mop:slot-definition-name slot)
          when (and (typep slot 'finalizable-slot)
                    (finalized slot))
          do (finalize (slot-value object slot-name))
             (slot-makunbound object slot-name))
    object))

(defmacro with-finalizing (bindings &body body)
  (let ((values (gensym "VALUES")))
    `(let ((,values ()))
       (unwind-protect
            (let ,(loop for (var def) in bindings
                        collect `(,var (let ((,var ,def))
                                         (push ,var ,values)
                                         ,var)))
              ,@body)
         (mapc #'finalize ,values)))))

(defmacro with-finalizing* (bindings &body body)
  (let ((values (gensym "VALUES")))
    `(let ((,values ()))
       (unwind-protect
            (let* ,(loop for (var def) in bindings
                         append `((,var ,def)
                                  (,values (push ,var ,values))))
              ,@body)
         (mapc #'finalize ,values)))))

(define-finalizable gc-finalized ()
  ((object :initarg :object :initform (error "OBJECT required.") :reader unbox :finalized T)))

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
