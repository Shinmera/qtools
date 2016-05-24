#|
This file is a part of Qtools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun to-method-name (thing)
  (etypecase thing
    (string thing)
    (symbol (capitalize-on #\- thing NIL))))

(defun qt-type-of (object)
  (typecase object
    (boolean "bool")
    (unsigned-byte "uint")
    (fixnum "int")
    (real "double")
    (complex "complex")
    (character "char")
    (string "const QString&")
    (widget "const QWidget&")
    (qobject "const QObject&")))

(defun qt-type-for (cl-type)
  (case cl-type
    (boolean "bool")
    (unsigned-byte "uint")
    (fixnum "int")
    (integer "int")
    (real "double")
    (complex "complex")
    (character "char")
    (string "const QString&")
    (widget "const QWidget&")
    (qobject "const QObject&")))

(defun to-type-name (thing)
  (etypecase thing
    (string thing)
    (symbol (or (qt-type-for thing)
                (string-downcase thing)))))

(defun cl-type-for (qt-type)
  (cond ((string-equal qt-type "bool") 'T)
        ((string-equal qt-type "int") 'fixnum)
        ((string-equal qt-type "double") 'real)
        ((string-equal qt-type "complex") 'complex)
        ((string-equal qt-type "char") 'character)
        ((string-equal qt-type "const QString&") 'string)
        ((string-equal qt-type "QString&") 'string)
        ((string-equal qt-type "QString") 'string)
        ((string-equal qt-type "string") 'string)
        ((string-equal qt-type "const QWidget&") 'qt-widget)
        ((string-equal qt-type "QWidget&") 'qt-widget)
        ((string-equal qt-type "QWidget") 'qt-widget)
        ((string-equal qt-type "const QObject&") 'qobject)
        ((string-equal qt-type "QObject&") 'qobject)
        ((string-equal qt-type "QObject") 'qobject)))

(defun eqt-type-of (object)
  (or (qt-type-of object)
      (error "No known C++ type for objects of type ~s." (type-of object))))

(defun ecl-type-for (qt-type)
  (or (cl-type-for qt-type)
      (error "No known CL type for type ~s." qt-type)))

(defun %determined-type-method-name-arg (stream arg a b)
  (declare (ignore a b))
  (write-string (if (listp arg)
                    (to-type-name (second arg))
                    (eqt-type-of arg))
                stream))

(defun determined-type-method-name (function args)
  (format NIL "~a(~{~/qtools::%determined-type-method-name-arg/~^, ~})"
          (to-method-name function) args))

(defun %specified-type-method-name-arg (stream arg a b)
  (declare (ignore a b))
  (write-string (to-type-name arg) stream))

(defun specified-type-method-name (function args)
  (format NIL "~a(~{~/qtools::%specified-type-method-name-arg/~^, ~})"
          (to-method-name function) args))
