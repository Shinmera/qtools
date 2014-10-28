#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

;;;;;
;; Qt Related Utils

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
    (v:trace :qtools "Copying QObject: ~a" instance))
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

(defun enumerate-method-descriptors (name args)
  (flet ((make-map (args)
           (format NIL "~a(~{~(~a~)~^, ~})" name args)))
    (cond
      ((and args (listp (first args)))
       (loop for i from 0 below (length (first args))
             collect (make-map (mapcar #'(lambda (list) (nth i list)) args))))
      (T
       (make-map args)))))

;;;;;
;; General utils

(defun ensure-class (thing)
  (etypecase thing
    (symbol (find-class thing))
    (class thing)
    (standard-object (class-of thing))))

(defmacro with-slots-bound ((instance class) &body body)
  (let ((slots (loop for slot in (c2mop:class-direct-slots
                                  (ensure-class class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

(defun fuse-plists (&rest plists-lists)
  (let ((target (make-hash-table)))
    (dolist (plists plists-lists)
      (loop for (option args) on plists by #'cddr
            do (setf (gethash option target)
                     (nconc (gethash option target) args))))
    (loop for key being the hash-keys of target
          for val being the hash-values of target
          appending (list key val))))

(defun fuse-alists (&rest alists-lists)
  (let ((target (make-hash-table)))
    (dolist (alists alists-lists)
      (loop for (option . args) in alists
            do (setf (gethash option target)
                     (append args (gethash option target)))))
    (loop for key being the hash-keys of target
          for val being the hash-values of target
          collect (cons key val))))

(defun split (list items &key (key #'identity) (test #'eql))
  (loop with table = ()
        for item in list
        do (push item (getf table (find (funcall key item) items :test test)))
        finally (return (cons (getf table NIL)
                              (loop for item in items
                                    collect (getf table item))))))
