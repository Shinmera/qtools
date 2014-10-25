#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *widget-init-priority* 10)
(defvar *slot-init-priority* 20)
(defvar *layout-init-priority* 100)

(defun fuse-plists (&rest plists-lists)
  (let ((target (make-hash-table)))
    (dolist (plists plists-lists)
      (loop for (option args) on plists by #'cddr
            do (setf (gethash option target)
                     (nconc (gethash option target) args))))
    (loop for key being the hash-keys of target
          for val being the hash-values of target
          appending (list key val))))

(defun canonicize-syntax-map (name args &rest extra)
  (flet ((make-map (args)
           `(,(format NIL "~a(~{~(~a~)~^, ~})" name args) ,@extra)))
    (cond
      ((and args (listp (first args)))
       (loop for i from 0 below (length (first args))
             collect (make-map (mapcar #'(lambda (list) (nth i list)) args))))
      (T
       `(,(make-map args))))))

(defclass qt-widget-class (finalizable-class qt-class)
  ((initializers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor qt-widget-initializers)
   (methods :initform (make-hash-table) :accessor qt-widget-methods)))

(defun add-initializer (class priority function)
  (vector-push-extend (cons priority function) (qt-widget-initializers class))
  (setf (qt-widget-initializers class)
        (sort (qt-widget-initializers class) #'> :key #'car)))

(defun compile-initializers (class)
  (loop for init across (qt-widget-initializers class)
        do (setf (cdr init) (compile NIL (funcall (cdr init) class)))))

(defun call-initializers (object)
  (loop for init across (qt-widget-initializers (ensure-class object))
        do (funcall (cdr init) object)))

(declaim (inline qt-widget-method))
(defun qt-widget-method (class name)
  (gethash name (qt-widget-methods (ensure-class class))))

(defun (setf qt-widget-method) (function class name)
  (setf (gethash name (qt-widget-methods (ensure-class class))) function))

(declaim (inline call-qt-widget-method))
(defun call-qt-widget-method (class name &rest args)
  (apply (qt-widget-method class name) args))

(defun compile-methods (class)
  (loop for name being the hash-keys of (qt-widget-methods class)
        for func being the hash-values of (qt-widget-methods class)
        do (setf (qt-widget-method class name)
                 (compile NIL (funcall func class)))))

(defgeneric process-qt-widget-option (option body class)
  (:method (option body class)
    (declare (ignore class))
    `(,option ,body)))

(defmacro define-qt-class-option ((option &optional (target option) (mode 'collect)) (class &rest body-lambda) &body forms)
  (assert (keywordp option) () "Option name must be a keyword.")
  (assert (or (null target) (keywordp target)) () "Target name must be a keyword.")
  (let* ((bodies (gensym "BODIES"))
         (body (gensym "BODY"))
         (inner-form `(loop for ,body in ,bodies
                            ,mode (destructuring-bind ,body-lambda ,body
                                    ,@forms))))
    `(defmethod process-qt-widget-option ((,(gensym "OPTION") (eql ,option)) ,bodies ,class)
       ,@(if target
             `((list ,target ,inner-form))
             `(,inner-form NIL)))))

(define-qt-class-option (:signal :signals append) (class name args)
  (canonicize-syntax-map (to-method-name name) args))

(define-qt-class-option (:slot :slots append) (class &rest body)
  (form-fiddle:with-destructured-lambda-form
      (:name name :lambda-list args :docstring doc :declarations decls :forms forms) (cons :slot body)
    (let* ((clean-decls (remove 'connected decls :key #'caadr :test #'eql))
           (connections (remove 'connected decls :key #'caadr :test-not #'eql))
           (clean-args (mapcar #'(lambda (a) (if (listp a) (car a) a)) args))
           (this (first clean-args)))
      (setf name (to-method-name name))

      (when connections
        (add-initializer
         class *slot-init-priority*
         #'(lambda (class)
             `(lambda (,this)
                (with-slots-bound (,this ,class)
                  ,@(loop for connection in connections
                          for args = (cdadr connection)
                          collect `(connect ,@args ,this ,name)))))))

      (setf (qt-widget-method class name)
            #'(lambda (class)
                `(lambda ,clean-args
                   ,@(when doc (list doc)) ,@clean-decls
                   (with-slots-bound (,(first clean-args) ,class)
                     ,@forms))))
      
      (canonicize-syntax-map
       name (mapcar #'cdr (cdr args))
       `(lambda (widget &rest args)
          (apply #'call-qt-widget-method widget ',name widget args))))))

(define-qt-class-option (:overrides :override) (class name args &rest body)
  (setf (qt-widget-method class name)
        #'(lambda (class)
            `(lambda ,args
               (with-slots-bound (,(first args) ,class)
                 ,@body))))
  
  `(,(to-method-name name)
    (lambda (widget &rest args)
      (apply #'call-qt-widget-method widget ',name widget args))))

(define-qt-class-option (:widget :direct-slots) (class name constructor &rest body)
  (add-initializer
   class *widget-init-priority*
   #'(lambda (class)
       `(lambda (this)
          (with-slots-bound (this ,class)
            (setf ,name ,constructor)
            ,@body))))
  
  `(:name ,name :readers () :writers () :initargs ()))

(define-qt-class-option (:layout NIL) (class name constructor &rest body)
  (add-initializer
   class *layout-init-priority*
   #'(lambda (class)
       `(lambda (this)
          (with-slots-bound (this ,class)
            (let ((,name ,constructor))
              ,@body
              (#_setLayout this ,name)))))))

;; We need to manually recreate this in order to ensure that
;; we can pass initargs that are not recognised in slots.
(defmethod make-instance ((class (eql (find-class 'qt-widget-class))) &rest initargs)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun initialize-qt-widget-class (class next &rest args)
  (setf (qt-widget-initializers class) (make-array 0 :adjustable T :fill-pointer 0)
        (qt-widget-methods class) (make-hash-table))
  (let ((args (apply #'fuse-plists
                     (loop for (option body) on args by #'cddr
                           collect (process-qt-widget-option option body class)))))
    (apply next class args)
    ;; compile stuff now that the class is ready.
    (compile-initializers class)
    (compile-methods class)))

(defmethod initialize-instance :around ((class qt-widget-class) &rest args)
  (apply #'initialize-qt-widget-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class qt-widget-class) &rest args)
  (apply #'initialize-qt-widget-class class #'call-next-method args))

(defclass qt-widget (finalizable)
  ()
  (:metaclass qt-widget-class))

(defmethod initialize-instance ((widget qt-widget) &key)
  (new widget)
  (call-initializers widget)
  (when (next-method-p)
    (call-next-method)))

(defun fuse-alists (&rest alists-lists)
  (let ((target (make-hash-table)))
    (dolist (alists alists-lists)
      (loop for (option . args) in alists
            do (setf (gethash option target)
                     (append args (gethash option target)))))
    (loop for key being the hash-keys of target
          for val being the hash-values of target
          collect (cons key val))))

(defmacro define-qt-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  `(defclass ,name (qt-widget ,@direct-superclasses)
     ,direct-slots
     (:metaclass qt-widget-class)
     (:qt-superclass ,(find-qt-class-name qt-class))
     ,@(fuse-alists options)))

(indent:define-indentation define-qt-widget
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

(defvar *special-form-option-map* (make-hash-table :test 'equalp))

(defun special-form-option (function)
  (gethash (string function) *special-form-option-map*))

(defun (setf special-form-option) (option function)
  (setf (gethash (string function) *special-form-option-map*) option))

(setf (special-form-option 'define-signal) :signal
      (special-form-option 'define-slot) :slot
      (special-form-option 'define-widget) :widget
      (special-form-option 'define-override) :overrides
      (special-form-option 'define-layout) :layout)

(defmacro define-qt-complex (&body forms)
  (loop for (function . body) in forms
        for option = (special-form-option function)
        if option
        collect `(,option ,body) into body-forms
        else
        collect `(,function ,@body) into other-forms
        finally (return
                  (let ((classdef (find 'define-qt-widget other-forms :key #'car)))
                    (if classdef
                        `(progn
                           ,(append classdef body-forms)
                           ,@(remove 'define-qt-widget other-forms :key #'car))
                        (progn
                          (when body-forms (warn "Class body forms found but no class definition!"))
                          `(progn
                             ,@other-forms)))))))
