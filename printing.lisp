#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defmacro print-unreadable-qobject ((instance stream &key type identity) &body body)
  `(print-unreadable-object (,instance ,stream :identity ,identity)
     ,@(when type `((format ,stream "Qt::~a " (qt:qclass-name (qt::qobject-class ,instance)))))
     ,@body))

(define-qclass-dispatch-function print print-qobject (instance stream))

(defmethod print-object ((instance qobject) stream)
  "Prints the qobject.

Use DESCRIBE-PRINT-METHOD for information on a specific printing mechanism.

Uses PRINT-OBJECT-USING-QCLASS and determines the class by QT::QOBJECT-CLASS."
  (print-qobject instance stream))

(defmethod no-applicable-method ((method (eql #'qclass-print-function)) &rest args)
  (destructuring-bind (instance stream) args
    (print-unreadable-qobject (instance stream :type T :identity T))))

(defmacro define-print-method ((instance class stream) &body body)
  "Defines a method to print an object of CLASS.
CLASS can be either a common-lisp class type or a Qt class name.

Qt class names will take precedence, meaning that if CLASS resolves
to a name using FIND-QT-CLASS-NAME a QCLASS-PRINT method
is defined on the respective qt-class. Otherwise a PRINT-OBJECT method
is defined with the CLASS directly as specializer for the instance.

In cases where you need to define a method on a same-named CL class,
directly use DEFMETHOD on PRINT-OBJECT.

See PRINT-OBJECT"
  (let ((qt-class-name (find-qt-class-name class)))
    (if qt-class-name
        `(define-qclass-print-function ,qt-class-name (,instance ,stream)
           ,@body)
        `(defmethod print-object ((,instance ,class) ,stream)
           ,@body))))

(define-print-method (instance QEvent stream)
  (print-unreadable-qobject (instance stream :type T :identity T)
    (format stream "~s ~a" :type (enum-value (#_type instance)))))

(define-print-method (instance QSize stream)
  (print-unreadable-qobject (instance stream :type T :identity T)
    (format stream "~s ~s ~s ~s" :width (#_width instance) :height (#_height instance))))

(define-print-method (instance QRect stream)
  (print-unreadable-qobject (instance stream :type T :identity T)
    (format stream "~s ~s ~s ~s ~s ~s ~s ~s"
            :x (#_x instance) :y (#_y instance) :width (#_width instance) :height (#_height instance))))

(define-print-method (instance QPoint stream)
  (print-unreadable-qobject (instance stream :type T :identity T)
    (format stream "~s ~s ~s ~s" :x (#_x instance) :y (#_y instance))))

(define-print-method (instance QColor stream)
  (print-unreadable-qobject (instance stream :type T :identity T)
    (format stream "~s ~s ~s ~s ~s ~s" :r (#_r instance) :g (#_g instance) :b (#_b instance))))

(defun describe-print-method (class)
  "Prints information about the print method for the specified class if possible."
  (let* ((qt-class-name (find-qt-class-name class))
         (method (if qt-class-name
                     (qclass-print-function qt-class-name)
                     (find-method #'print-object () `(,(ensure-class class))))))
    (if method
        (format T "Print method for ~:[CL class~;Qt class~] ~a.~%~:[No docstring specified.~;~:*~s~]~%"
                qt-class-name class (documentation method T))
        (format T "No print method for the given class found.~%"))))
