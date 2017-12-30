#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defclass finalizable-class (standard-class)
  ())

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass finalizable-class))
  T)

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class finalizable-class) (superclass finalizable-class))
  T)

(defclass finalizable-slot ()
  ((finalized :initarg :finalized :initform NIL :reader finalized)))

(defmethod print-object ((slot finalizable-slot) stream)
  (print-unreadable-object (slot stream :type T :identity T)
    (format stream "~s~@[ finalized~*~]" (c2mop:slot-definition-name slot) (finalized slot))))

(defclass finalizable-direct-slot-definition (finalizable-slot c2mop:standard-direct-slot-definition)
  ())

(defclass finalizable-effective-slot-definition (finalizable-slot c2mop:standard-effective-slot-definition)
  ())

;; For some reason the slot doesn't get set? (?????)
(defmethod c2mop:compute-effective-slot-definition ((class finalizable-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'finalizable-direct-slot-definition)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (setf (slot-value effective-slot 'finalized)
                     (finalized direct-slot))
               (return)))
    effective-slot))

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
  (when (loop for name in direct-superclasses
              for superclass = (find-class name NIL)
              never (and superclass (c2mop:subclassp superclass (find-class 'finalizable))))
    (push 'finalizable direct-superclasses))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       (:metaclass finalizable-class)
       ,@options)))

(define-qclass-dispatch-function finalize finalize-qobject (instance))

(defgeneric finalize (object)
  (:method (object)
    object)
  (:method ((object qobject))
    (call-next-method)
    (finalize-qobject object)))

(defmacro define-finalize-method ((instance class) &body body)
  (let ((qt-class-name (find-qt-class-name class)))
    (if qt-class-name
        `(define-qclass-finalize-function ,qt-class-name (,instance)
           ,@body)
        `(defmethod finalize ((,instance ,class))
           ,@body))))

;; Fall-through as FINALIZE should always have a last "do nothing" method.
(defmethod no-applicable-method ((method (eql #'qclass-finalize-function)) &rest args)
  (declare (ignore method))
  (first args))

(defmethod no-next-method ((disp (eql 'finalize)) method &rest args)
  (declare (ignore disp method))
  (first args))

(define-finalize-method (object QPaintDevice)
  "Errors if there are still painters active on the paint device."
  (when (and (qobject-alive-p object)
             (#_paintingActive object))
    (error "Cannot finalize ~a, there are active painters!" object))
  (call-next-method))

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

(define-finalize-method (object hash-table)
  "Calls the next method and then invokes FINALIZE on all the keys and values of the table."
  (call-next-method)
  (maphash #'(lambda (a b) (finalize a) (finalize b)) object)
  object)

(define-finalize-method (object vector)
  "Calls the next method and then invokes FINALIZE on all items of the vector."
  (call-next-method)
  (unless (stringp object)
    (loop for item across object
          do (finalize item)))
  object)

(define-finalize-method (object list)
  "Calls the next method and then invokes FINALIZE on all items of the list."
  (call-next-method)
  (dolist (item object)
    (finalize item))
  object)

(define-finalize-method (object finalizable)
  "Finalizes and unbinds all slots on the object that are marked as FINALIZED and then calls the next method."
  #+:verbose (v:debug :qtools "Finalizing slots on ~s" object)
  (loop for slot in (c2mop:class-slots (class-of object))
        for slot-name = (c2mop:slot-definition-name slot)
        when (and (typep slot 'finalizable-slot)
                  (finalized slot)
                  (slot-boundp object slot-name))
        do (finalize (slot-value object slot-name))
           (slot-makunbound object slot-name))
  (call-next-method)
  object)

(defun describe-finalize-method (class)
  (let* ((qt-class-name (find-qt-class-name class))
         (method (if qt-class-name
                     (qclass-finalize-function qt-class-name)
                     (find-method #'finalize () `(,(ensure-class class))))))
    (if method
        (format T "Finalize method for ~:[CL class~;Qt class~] ~a.~%~:[No docstring specified.~;~:*~s~]~%"
                qt-class-name class (documentation method T))
        (format T "No finalize method for the given class found.~%"))))

(defun normalize-bindings (bindings)
  (loop for bind in bindings
        collect (etypecase bind
                  (symbol (list bind NIL))
                  (list (if (cddr bind)
                            (error "Invalid binding spec: ~s" bind)
                            bind)))))

(defun %build-with-finalizing-construct (binder bindings body)
  (let* ((bindings (normalize-bindings bindings))
         (temporaries (mapcar (lambda (a) (gensym (string (car a)))) bindings)))
    `(let ,temporaries
       (unwind-protect
            (,binder ,(loop for (var val) in bindings
                         for temp in temporaries
                         append `((,var (setf ,temp ,val))))
              ,@body)
         ,@(loop for temp in (reverse temporaries)
                 collect `(finalize ,temp))))))

(defmacro with-finalizing (bindings &body body)
  (%build-with-finalizing-construct 'let bindings body))

(defmacro with-finalizing* (bindings &body body)
  (%build-with-finalizing-construct 'let* bindings body))
