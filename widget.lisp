#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

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
  ((initializers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor qt-widget-initializers)))

(defun add-initializer (class priority function)
  (vector-push-extend (cons priority function) (qt-widget-initializers class))
  (setf (qt-widget-initializers class)
        (sort (qt-widget-initializers class) #'> :key #'car)))

(defun call-initializers (object)
  (loop for init across (qt-widget-initializers (if (typep object 'qt-widget-class)
                                                    object (class-of object)))
        do (funcall (cdr init) object)))

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
           (self (first clean-args))
           (body `(lambda ,clean-args ,@(when doc (list doc)) ,@clean-decls
                          (with-class-bindings (,(first clean-args) ',class)
                            ,forms))))
      (setf name (to-method-name name))
      
      (add-initializer
       class *slot-init-priority*
       #'(lambda (class)
           `(lambda (,self)
              (with-class-bindings (,self ,class)
                ,@(loop for connection in connections
                        for args = (cdadr connection)
                        collect `(connect ,@args ,self ,name))))))
      
      (canonicize-syntax-map name (mapcar #'cdr (cdr args)) body))))

(define-qt-class-option (:overrides :override) (class name args &rest body)
  `(,(to-method-name name)
    (lambda ,args
      (with-class-bindings (,(first args) ,class)
        ,@body))))

(define-qt-class-option (:widget :direct-slots) (class name constructor &rest body)
  (add-initializer
   class *widget-init-priority*
   #'(lambda (class)
       `(lambda (this)
          (with-class-bindings (this ,class)
            (setf ,name ,constructor)
            ,@body))))
  
  `(:name ,name :readers () :writers () :initargs ()))

(define-qt-class-option (:layout NIL) (class name constructor &rest body)
  (add-initializer
   class *layout-init-priority*
   #'(lambda (class)
       `(lambda (this)
          (with-class-bindings (this ,class)
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
  (setf (qt-widget-initializers class)
        (make-array 0 :adjustable T :fill-pointer 0))
  (let ((args (apply #'fuse-plists
                     (loop for (option body) on args by #'cddr
                           collect (process-qt-widget-option option body class)))))
    (apply next class args)
    ;; Compile the initializers
    (loop for init across (qt-widget-initializers class)
          do (setf (cdr init) (compile NIL (funcall (cdr init) class))))))

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
        for class-type = (resolve-class-type function)
        if class-type
        collect `(,class-type ,body) into body-forms
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
