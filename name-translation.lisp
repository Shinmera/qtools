#|
This file is a part of Qtools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun to-method-name (thing)
  "Turns THING into a Qt method name.
If THING is a STRING, it is returned directly.
If THING is a SYMBOL, it is transformed by turning each
character after a hyphen into its uppercase equivalent
and dropping the hyphen. Therefore: foo-bar fooBar"
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
  "Attempts to determine a proper Qt type descriptor for the type of the OBJECT.

Look at the source to see the mappings."
  (typecase object
    (boolean "bool")
    (unsigned-byte "uint")
    (fixnum "int")
    (real "double")
    (complex "complex")
    (character "char")
    (string "const QString&")
    (widget "QWidget&")
    (qobject "QObject&")))

(defun qt-type-for (cl-type)
  "Attempts to determine the proper Qt type descriptor for the passed cl type name.

Look at the source to see the mappings."
  (case cl-type
    (boolean "bool")
    (unsigned-byte "uint")
    (fixnum "int")
    (integer "int")
    (real "double")
    (complex "complex")
    (character "char")
    (string "const QString&")
    (widget "QWidget&")
    (qobject "QObject&")))

(defun to-type-name (thing)
  "Returns the type name for THING.

If THING is a string, it is returned directly.
If it is a symbol, either QT-TYPE-FOR for THING is
returned, or the STRING-DOWNCASE of THING."
  (etypecase thing
    (string thing)
    (symbol (or (qt-type-for thing)
                (string-downcase thing)))))

(defun cl-type-for (qt-type)
  "Attempts to determine the CL type for the given Qt type descriptor.

Look at the source to see the mappings."
  (cond ((string-equal qt-type "bool") 'boolean)
        ((string-equal qt-type "int") 'fixnum)
        ((string-equal qt-type "double") 'real)
        ((string-equal qt-type "complex") 'complex)
        ((string-equal qt-type "char") 'character)
        ((string-equal qt-type "const QString&") 'string)
        ((string-equal qt-type "QString&") 'string)
        ((string-equal qt-type "QWidget&") 'qt-widget)
        ((string-equal qt-type "QObject&") 'qobject)))

(defun eqt-type-of (object)
  "Same as QT-TYPE-OF, but signals an error if no matching type could be found."
  (or (qt-type-of object)
      (error "No known C++ type for objects of type ~s." (type-of object))))

(defun ecl-type-for (qt-type)
  "Same as CL-TYPE-FOR, but signals an error if no matching type could be found."
  (or (cl-type-for qt-type)
      (error "No known CL type for type ~s." qt-type)))

(defun %determined-type-method-name-arg (stream arg a b)
  (declare (ignore a b))
  (write-string (if (consp arg)
                    (to-type-name (cdr arg))
                    (eqt-type-of arg))
                stream))

(defun determined-type-method-name (function args)
  "Returns a method designator for the FUNCTION and ARGS.

The FUNCTION is transformed as by TO-METHOD-NAME.
Argument types are determined as follows:
If the argument is a CONS, the CAR is taken as a value (and thus discarded)
and the CDR is the literal type to take. Otherwise the type is determined
by EQT-TYPE-OF."
  (format NIL "~a(~{~/qtools::%determined-type-method-name-arg/~^, ~})"
          (to-method-name function) args))

(defun %specified-type-method-name-arg (stream arg a b)
  (declare (ignore a b))
  (write-string (to-type-name arg) stream))

(defun specified-type-method-name (function args)
  "Returns a method designator for the FUNCTION and ARGS.

The FUNCTION is transformed as by TO-METHOD-NAME. Each argument type is
determined as by TO-TYPE-NAME."
  (format NIL "~a(~{~/qtools::%specified-type-method-name-arg/~^, ~})"
          (to-method-name function) args))
