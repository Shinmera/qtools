#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defgeneric copy-using-class (qclass instance)
  (:documentation "Creates a copy of the given instance by using methods
appropriate for the given qclass.

See COPY")
  #+:verbose
  (:method :before (qclass instance)
    (v:trace :qtools "Copying: ~a" instance)))

(defgeneric copy (instance)
  (:documentation "Generates a copy of the object.

The way objects are copied varies, but usually it can be assumed that the
copy is made in a way such that data immediately associated with the object
is copied (such as pixel data in an image), but data only implicitly
referenced (such as the paint device of a painter) is not.

Use DESCRIBE-COPY-METHOD for information on a specific copying mechanism.

Uses COPY-QOBJECT-USING-CLASS and determines the class by QT::QOBJECT-CLASS.")
  (:method (instance)
    (copy-using-class
     (qt::qobject-class instance) instance)))

(defmacro define-copy-method ((instance class) &body body)
  "Defines a method to copy an object of CLASS.
CLASS can be either a common-lisp class type or a Qt class name.

Qt class names will take precedence, meaning that if CLASS resolves
to a name using FIND-QT-CLASS-NAME a COPY-QOBJECT-USING-CLASS method
is defined on the respective qt-class. Otherwise a COPY-QOBJECT method
is defined with the CLASS directly as specializer for the instance.

In cases where you need to define a method on a same-named CL class,
directly use DEFMETHOD on COPY-QOBJECT.

See COPY-QOBJECT, COPY-QOBJECT-USING-CLASS"
  (let ((qclass (gensym "QCLASS"))
        (qt-class-name (find-qt-class-name class)))
    (if qt-class-name
        `(defmethod copy-using-class ((,qclass (eql (find-qclass ,qt-class-name))) ,instance)
           (declare (ignore ,qclass))
           ,@body)
        `(defmethod copy ((,instance ,class))
           ,@body))))

(define-copy-method (instance QBrush)
  "Returns a copy of QBrush using QBrush::QBrush."
  (#_new QBrush instance))

(define-copy-method (instance QColor)
  "Creates a new QColor using QColor::QColor."
  (#_new QColor instance))

(define-copy-method (instance QImage)
  "Uses QImage::copy to produce a copy."
  (#_copy instance (#_rect instance)))

(define-copy-method (instance QPainter)
  "Copies the QPainter by creating a new QPainter with the same device."
  (#_new QPainter (#_device instance)))

(define-copy-method (instance QPalette)
  "Shallow-copies QPalette using QPalette::QPalette."
  (#_new QPalette instance))

(define-copy-method (instance QPen)
  "Creates a new QPen using the QPen::QPen."
  (#_new QPen instance))

(define-copy-method (instance QPixmap)
  "Creates a new QPixmap using QPixmap::copy (deep copy)."
  (#_copy instance (#_rect instance)))

(define-copy-method (instance QTransform)
  "Generates a new QTransform copy by copying the transform matrix whole."
  (#_new QTransform
         (#_m11 instance) (#_m12 instance) (#_m13 instance)
         (#_m21 instance) (#_m22 instance) (#_m23 instance)
         (#_m31 instance) (#_m32 instance) (#_m33 instance)))

(define-copy-method (instance gc-finalized)
  "Creates a new GC-Finalized object using the value of COPY on its contained object."
  (make-gc-finalized (copy (unbox instance))))

(defun describe-copy-method (class)
  "Prints information about the copy method for the specified class if possible."
  (let* ((qt-class-name (find-qt-class-name class))
         (method (if qt-class-name
                     (find-method #'copy-using-class () `((eql ,(find-qclass qt-class-name)) T))
                     (find-method #'copy () `(,(ensure-class class))))))
    (if method
        (format T "Copy method for ~:[CL class~;Qt class~] ~a.~%~:[No docstring specified.~;~:*~s~]~%"
                qt-class-name class (documentation method T))
        (format T "No copy method for the given class found.~%"))))
