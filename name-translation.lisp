#|
This file is a part of Qtools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun to-method-name (thing)
  (etypecase thing
    (string thing)
    (symbol (with-output-to-string (stream)
              (loop with capitalize = NIL
                    for char across (string-downcase thing)
                    do (cond ((char= char #\-)
                              (setf capitalize T))
                             (capitalize
                              (write-char (char-upcase char) stream)
                              (setf capitalize NIL))
                             (T
                              (write-char char stream))))))))

(defun qt-type-of (object)
  (typecase object
    (boolean "bool")
    (unsigned-byte "uint")
    ((integer -2147483648 2147483647) "int")
    (real "double")
    (complex "complex")
    (character "char")
    (string "const QString&")
    (qt-widget "QWidget&")
    (qobject "QObject&")))

(defun qt-type-for (cl-type)
  (case cl-type
    (boolean "bool")
    (integer "int")
    (unsigned-byte "uint")
    (real "double")
    (complex "complex")
    (character "char")
    (string "const QString&")
    (qt-widget "QWidget&")
    (qobject "QObject&")))

(defun to-type-name (thing)
  (etypecase thing
    (string thing)
    (symbol (or (qt-type-for thing)
                (string-downcase thing)))))

(defun cl-type-for (qt-type)
  (cond ((string-equal qt-type "bool") 'boolean)
        ((string-equal qt-type "int") '(integer -2147483648 2147483647))
        ((string-equal qt-type "double") 'real)
        ((string-equal qt-type "complex") 'complex)
        ((string-equal qt-type "char") 'character)
        ((string-equal qt-type "const QString&") 'string)
        ((string-equal qt-type "QWidget&") 'qt-widget)
        ((string-equal qt-type "QObject&") 'qobject)))

(defun eqt-type-of (object)
  (or (qt-type-of object)
      (error "No known C++ type for objects of type ~s." (type-of object))))

(defun ecl-type-for (qt-type)
  (or (cl-type-for qt-type)
      (error "No known CL type for type ~s." qt-type)))

(defun %determined-type-method-name-arg (stream arg a b)
  (declare (ignore a b))
  (write-string (if (consp arg)
                    (cdr arg)
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
