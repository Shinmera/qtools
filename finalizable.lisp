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

(defclass finalizable-direct-slot-definition (c2mop:standard-direct-slot-definition finalizable-slot)
  ())

(defclass finalizable-effective-slot-definition (c2mop:standard-effective-slot-definition finalizable-slot)
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
          when (and (typep slot 'finalizable-slot)
                    (finalized slot))
          do (finalize (slot-value object (c2mop:slot-definition-name slot))))
    object))
