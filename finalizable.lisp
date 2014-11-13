#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defclass finalizable-class (standard-class)
  ()
  (:documentation "Metaclass for classes with finalizable slots."))

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass finalizable-class))
  T)

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass finalizable-class))
  T)

(defclass finalizable-slot ()
  ((finalized :initarg :finalized :initform NIL :reader finalized))
  (:documentation "Superclass for slots with a finalized option."))

(defmethod print-object ((slot finalizable-slot) stream)
  (print-unreadable-object (slot stream :type T :identity T)
    (format stream "~s~@[ finalized~*~]" (c2mop:slot-definition-name slot) (finalized slot))))

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
  (:metaclass finalizable-class)
  (:documentation "A class for finalizable objects."))

(defmacro define-finalizable (name direct-superclasses direct-slots &rest options)
  "Shorthand around DEFCLASS to create a finalizable class.

Automatically adds FINALIZABLE as direct-superclass and 
FINALIZABLE-CLASS as metaclass."
  `(defclass ,name (finalizable ,@direct-superclasses)
     ,direct-slots
     (:metaclass finalizable-class)
     ,@options))

(defgeneric finalize-using-class (class object)
  (:documentation "Extension to the FINALIZE generic function to support differentiating by the
classes of an object, mostly used for native QObjects as they don't integrate into CLOS directly.
You need to use EQL specializers on the class, as a Qt class is represented as an integer.

E.g: (defmethod finalize-using-class ((class (eql (find-class \"QWidget\"))) object) ..)

This method should not be called directly.

See FINALIZE")
  (:method (class object)
    (declare (ignore class))
    object))

(defgeneric finalize (object)
  (:documentation "Finalizes the object. The effects thereof may vary and even result in nothing at all.
After FINALIZE has been called on an object, it should not be attempted to be used in any fashion
whatsoever as it may have been rendered unusable or unstable.

This method should be called on any object once it is known that it can be discarded.
FINALIZE will then try to clean up objects and make sure that they don't clutter your
memory, as lingering QOBJECTs would.")
  #+:verbose
  (:method :before (object)
    (v:trace :qtools "Finalizing ~s" object))
  (:method (object)
    object)
  (:method ((object qobject))
    (call-next-method)
    (finalize-using-class (qt::qobject-class object) object)))

(defmacro define-finalize-method ((instance class) &body body)
  "Defines a method to finalize an object of CLASS.
CLASS can be either a common-lisp class type or a Qt class name.

Qt class names will take precedence, meaning that if CLASS resolves
to a name using FIND-QT-CLASS-NAME a COPY-QOBJECT-USING-CLASS method
is defined on the respective qt-class. Otherwise a COPY-QOBJECT method
is defined with the CLASS directly as specializer for the instance.

In cases where you need to define a method on a same-named CL class,
directly use DEFMETHOD on FINALIZE.

See FINALIZE, FINALIZE-USING-CLASS"
  (let ((qclass (gensym "QCLASS"))
        (qt-class-name (find-qt-class-name class)))
    (if qt-class-name
        `(defmethod finalize-using-class ((,qclass (eql (find-qclass ,qt-class-name))) ,instance)
           (declare (ignore ,qclass))
           ,@body)
        `(defmethod finalize ((,instance ,class))
           ,@body))))

(define-finalize-method (object QPainter)
  "Calls the next method and then invokes QPainter::end."
  (when (qobject-alive-p object)
    (#_end object))
  (call-next-method))

(define-finalize-method (object abstract-qobject)
  "Calls the next method and then invokes MAYBE-DELETE-QOBJECT."
  (call-next-method)
  (maybe-delete-qobject object)
  object)

(define-finalize-method (object finalizable)
  "Calls the next method and then finalizes and unbinds all slots on the object that are marked as FINALIZED."
  (call-next-method)
  (loop for slot in (c2mop:class-direct-slots (class-of object))
        for slot-name = (c2mop:slot-definition-name slot)
        when (and (typep slot 'finalizable-slot)
                  (finalized slot))
        do (finalize (slot-value object slot-name))
           (slot-makunbound object slot-name))
  object)

(defun describe-finalize-method (class)
  "Prints information about the finalize method for the given class if possible."
  (let* ((qt-class-name (find-qt-class-name class))
         (method (if qt-class-name
                     (find-method #'finalize-using-class () `((eql ,(find-qclass qt-class-name)) T))
                     (find-method #'finalize () `(,(ensure-class class))))))
    (if method
        (format T "Finalize method for ~:[CL class~;Qt class~] ~a.~%~:[No docstring specified.~;~:*~s~]~%"
                qt-class-name class (documentation method T))
        (format T "No finalize method for the given class found.~%"))))

(defmacro with-finalizing (bindings &body body)
  "Executes the body as by LET and calls FINALIZE on all the objects introduced by
the bindings on completion of the body. If an error occurs during the binding phase,
all objects bound up until that point are still finalized."
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
  "Executes the body as by LET* and calls FINALIZE on all the objects introduced by
the bindings on completion of the body. If an error occurs during the binding phase,
all objects bound up until that point are still finalized."
  (let ((values (gensym "VALUES")))
    `(let ((,values ()))
       (unwind-protect
            (let* ,(loop for (var def) in bindings
                         append `((,var ,def)
                                  (,values (push ,var ,values))))
              ,@body)
         (mapc #'finalize ,values)))))
