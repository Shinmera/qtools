#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defmacro print-unreadable-qobject ((object stream &key type identity) &body body)
  (let ((instance (gensym "INSTANCE"))
        (output (gensym "OUTPUT")))
    `(let ((,instance ,object)
           (,output ,stream))
       (print-unreadable-object (,instance ,output :identity ,identity)
         ,@(when type `((format ,stream "Qt::~a " (qt:qclass-name (qt::qobject-class ,instance)))))
         (cond ((qobject-alive-p ,instance)
                ,@body)
               (T
                (format ,output "~s" :deleted)))))))

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
    (format stream "~s ~s ~s ~s ~s ~s" :r (#_red instance) :g (#_green instance) :b (#_blue instance))))

(defun describe-print-method (class)
  (let* ((qt-class-name (find-qt-class-name class))
         (method (if qt-class-name
                     (qclass-print-function qt-class-name)
                     (find-method #'print-object () `(,(ensure-class class))))))
    (if method
        (format T "Print method for ~:[CL class~;Qt class~] ~a.~%~:[No docstring specified.~;~:*~s~]~%"
                qt-class-name class (documentation method T))
        (format T "No print method for the given class found.~%"))))
