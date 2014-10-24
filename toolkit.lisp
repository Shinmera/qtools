#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun qobject-alive-p (object)
  (not (or (null-qobject-p object)
           (qobject-deleted object))))

(defun maybe-delete-qobject (object)
  (if (typep object 'abstract-qobject)
      (when (qobject-alive-p object)
        #+:verbose (v:trace :qtools "Deleting QObject: ~a" object)
        (optimized-delete object))
      #+:verbose (v:trace :qtools "Deleting QObject: WARN Tried to delete non-qobject ~a" object)))

(defgeneric copy-qobject (qclass instance)
  #+:verbose
  (:method :before (qclass instance)
    (v:trace tools "Copying QObject: ~a" instance))
  ;; QImage
  (:method ((qclass (eql 11848)) instance) 
    (#_copy instance))
  ;; QColor
  (:method ((qclass (eql 3976)) instance)
    (#_new QColor instance)))

(defmacro qtenumcase (keyform &body forms)
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond ,@(loop for form in forms
                     collect `((qt:enum= ,key ,(car form)) ,@(cdr form)))))))
