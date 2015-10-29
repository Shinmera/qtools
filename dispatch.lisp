#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defun qclass-class-list (qclass)
  (loop for classes = (list (ensure-qclass qclass))
        then (let ((new ()))
               (loop for class in classes
                     do (qt::map-qclass-direct-superclasses
                         (lambda (class) (push class new)) class))
               (nreverse new))
        while classes
        nconc classes))

(defun qclass-precedence-set (qclass)
  (let ((set ()))
    (labels ((recurse (class)
               (let ((prev class))
                 (qt::map-qclass-direct-superclasses
                  (lambda (class)
                    (push (cons prev class) set)
                    (recurse class)
                    (setf prev class))
                  class))))
      (recurse (ensure-qclass qclass)))
    (nreverse set)))

(defun direct-qsubclass-p (qclass maybe-superclass)
  (let ((maybe-superclass (ensure-qclass maybe-superclass)))
    (qt::map-qclass-direct-superclasses
     (lambda (super)
       (when (eql super maybe-superclass)
         (return-from direct-qsubclass-p T)))
     (ensure-qclass qclass)))
  NIL)

(defun compute-qclass-precedence-list (qclass)
  (let ((classes (qclass-class-list qclass))
        (precedence (qclass-precedence-set qclass))
        (list ()))
    (labels ((qclass-order (superclass)
               (loop for i from 0
                     for class in list
                     when (direct-qsubclass-p class superclass)
                     return i))
             (find-next-classes ()
               (loop until (or (not classes)
                               (find (car classes) precedence :key #'cdr))
                     for class = (pop classes)
                     collect (cons class (qclass-order class)))))
      (loop while classes
            do (let ((next (find-next-classes)))
                 ;; Rightmost subclass goes first.
                 (setf next (sort next #'> :key #'cdr))
                 ;; Remove from precedence and add to final ordering.
                 (dolist (n next)
                   (setf precedence (delete (car n) precedence :key #'car))
                   (push (car n) list)))))
    (nreverse list)))

(defvar *qclass-precedence-lists* (make-hash-table :test 'eql))
(defun qclass-precedence-list (qclass)
  (let ((qclass (ensure-qclass qclass)))
    (or (gethash qclass *qclass-precedence-lists*)
        (setf (gethash qclass *qclass-precedence-lists*)
              (compute-qclass-precedence-list qclass)))))

(defvar *qclass-precedence-list* ())
(defun dispatch-by-qclass (method-locator object &rest args)
  (loop for *qclass-precedence-list* on (qclass-precedence-list object)
        for class = (first *qclass-precedence-list*)
        for method = (funcall method-locator class)
        when method return (apply method object args)
        finally (apply #'no-applicable-method method-locator object args)))

(defun generate-qclass-dispatch-lambda (qclass fun basename args body)
  `(named-lambda ,qclass ,args
     (flet ((next-method-p ()
              (loop for class in (rest *qclass-precedence-list*)
                    thereis (,fun class)))
            (call-next-method ()
              (let ((precedence *qclass-precedence-list*))
                (loop for *qclass-precedence-list* on (rest precedence)
                      for class = (first *qclass-precedence-list*)
                      for method = (,fun class)
                      when method return (funcall method ,@args)
                      finally (no-next-method ',basename (,fun (first precedence)) ,@args)))))
       (declare (ignorable #'next-method-p #'call-next-method))
       ,@body)))

(defmacro define-qclass-dispatch-function (basename dispatcher args)
  (let ((var (intern (format NIL "*QCLASS-~a-FUNCTIONS*" (string-upcase basename))))
        (fun (intern (format NIL "QCLASS-~a-FUNCTION" (string-upcase basename))))
        (rem (intern (format NIL "REMOVE-QCLASS-~a-FUNCTION" (string-upcase basename))))
        (def (intern (format NIL "DEFINE-QCLASS-~a-FUNCTION" (string-upcase basename)))))
    `(progn
       (defvar ,var (make-hash-table :test 'equal))

       (defun ,fun (qclass)
         (etypecase qclass
           (fixnum
            (gethash (qclass-name qclass) ,var))
           ((or symbol string)
            (gethash (ensure-completable-qclass qclass) ,var))))

       (defun (setf ,fun) (function qclass)
         (setf (gethash (ensure-completable-qclass qclass) ,var)
               function))

       (defun ,rem (qclass)
         (remhash (ensure-qclass qclass) ,var))

       (defmacro ,def (qclass args &body body)
         `(setf (,',fun ,qclass)
                ,(generate-qclass-dispatch-lambda qclass ',fun ',basename args body)))

       (defmethod no-next-method ((fun (eql ',basename)) func &rest args)
         (error "There is no next method for the qclass dispatch function~&~a~&when called from method~&~a~&with arguments~&~a."
                ',basename func args))

       (defun ,dispatcher ,args
         (dispatch-by-qclass #',fun ,@args)))))
