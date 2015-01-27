(defpackage #:qtools-alternate
  (:nicknames #:org.shirakumo.qtools.alternate)
  (:use #:cl)
  (:shadow #:defmethod))
(in-package #:org.shirakumo.qtools.alternate)
(named-readtables:in-readtable :qtools)

(defclass widget-class (qtools:finalizable-class qt:qt-class)
  ((direct-options :initform () :accessor widget-class-direct-options)
   (extern-options :initform () :accessor widget-class-extern-options)
   (initializers :initform () :accessor widget-class-initializers)
   (finalizers :initform () :accessor widget-class-finalizers)))

(defclass widget (qtools:finalizable)
  ()
  (:metaclass widget-class)
  (:qt-superclass "QObject"))

(defun call-initializers (class)
  (mapc #'(lambda (function) (funcall function class))
        (widget-class-initializers (qtools:ensure-class class)))
  class)

(defun call-finalizers (class)
  (mapc #'(lambda (function) (funcall function class))
        (widget-class-finalizers (qtools:ensure-class class)))
  class)

(cl:defmethod initialize-instance ((widget widget) &key)
  (when (next-method-p)
    (call-next-method))
  (qt:new widget)
  (call-initializers widget))

(cl:defmethod qtools:finalize :before ((widget widget))
  (call-finalizers widget))

(defun setup-widget-class (class next-method &rest options &key initializers finalizers (save-direct-options T) &allow-other-keys)
  (declare (ignore initializers finalizers))
  ;; Append extra options
  (when (slot-boundp class 'extern-options)
    (loop for (name value) on (widget-class-extern-options class) by #'cddr
          do (setf (getf options name) (append (getf options name) value))))
  (let ((initializers (getf options :initializers))
        (finalizers (getf options :finalizers)))
    ;; Delegate
    (remf options :initializers)
    (remf options :finalizers)
    (remf options :save-direct-options)
    (print options)
    (apply next-method class options)
    ;; Now that the class is ready, process init/finalizers
    (flet ((sort-clean (list)
             (mapcar #'third (stable-sort (copy-list list) #'> :key #'second))))
      (setf (widget-class-initializers class) (sort-clean initializers))
      (setf (widget-class-finalizers class) (sort-clean finalizers)))
    ;; Save directly specified options
    (when save-direct-options
      (setf (widget-class-direct-options class) options))))

(cl:defmethod initialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method options))

(cl:defmethod reinitialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method options))

(defun softly-redefine-widget-class (class)
  (let ((class (qtools:ensure-class class)))
    ;; Press new options into the class definition
    (apply #'reinitialize-instance class :save-direct-options NIL (copy-list (widget-class-direct-options class)))
    ;; CommonQt performs computations on finalisation
    (c2mop:finalize-inheritance class)))

(defun set-widget-class-option (class option value)
  (let* ((identifier (car value))
         (class (qtools:ensure-class class))
         (idents (getf (widget-class-extern-options class) option)))
    (setf (getf (widget-class-extern-options class) option)
          (cons value (remove identifier idents :key #'first :test #'equal)))
    (softly-redefine-widget-class class)))

(defun remove-widget-class-option (class option identifier)
  (let ((class (qtools:ensure-class class)))
    (setf (getf (widget-class-extern-options class) option)
          (remove identifier (getf (widget-class-extern-options class) option)
                  :key #'first :test #'equal))
    (softly-redefine-widget-class class)))

(defmacro define-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'widget)))
    (push 'widget direct-superclasses))
  `(defclass ,name ,direct-superclasses
     ,direct-slots
     (:metaclass widget-class)
     (:qt-superclass ,(qtools:find-qt-class-name qt-class))
     ,@options))

(defvar *method*)
(defvar *method-declarations* (make-hash-table :test 'eql))

(defun method-declaration (name)
  (gethash name *method-declarations*))

(defun (setf method-declaration) (function name)
  (setf (gethash name *method-declarations*) function))

(defun remove-method-declaration (name)
  (remhash name *method-declarations*))

(defmacro define-method-declaration (name args &body body)
  `(setf (method-declaration ',name)
         #'(lambda ,args ,@body)))

(define-method-declaration slot (name args)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    (let ((slot (qtools:specified-type-method-name name args)))
      `(set-widget-class-option ',(second (first lambda)) :slots '(,slot ,method)))))

(define-method-declaration override (&optional name)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    (let ((slot (qtools:to-method-name (or name method))))
      `(set-widget-class-option ',(second (first lambda)) :override '(,slot ,name)))))

(define-method-declaration initializer (priority)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    `(set-widget-class-option ',(second (first lambda)) :initializers '(,method ,priority ,method))))

(define-method-declaration finalizer (priority)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    `(set-widget-class-option ',(second (first lambda)) :finalizers '(,method ,priority ,method))))

(defmacro defmethod (&whole whole name &rest args)
  (declare (ignore name args))
  (destructuring-bind (function name qualifiers lambda-list docstring declarations forms) (form-fiddle:split-lambda-form whole)
    (declare (ignore function))
    (let ((declaration-forms)
          (unknown-declarations)
          (*method* whole))
      (loop for declaration in declarations
            for (name . args) = (second declaration)
            for declaration-function = (method-declaration name)
            do (if declaration-function
                   (push (apply declaration-function args) declaration-forms)
                   (push declaration unknown-declarations)))
      `(progn
         (cl:defmethod ,name ,@qualifiers ,lambda-list
           ,@(when docstring (list docstring))
           ,@unknown-declarations
           ,@forms)
         ,@declaration-forms
         ',name))))

(defmacro define-slot ((widget-class method-name &optional (slot method-name)) args &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class) ,@(mapcar #'first args))
     (declare (slot ,slot ,(mapcar #'second args)))
     ,@body))

(defmacro define-override ((widget-class method-name &optional (override method-name)) args &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class) ,args)
     (declare (override ,override))
     ,@body))

(defmacro define-initializer ((widget-class method-name &optional (priority 0)) &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class))
     (declare (initializer ,priority))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-finalizer ((widget-class method-name &optional (priority 0)) &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class))
     (declare (finalizer ,priority))
     (qtools:with-slots-bound (,widget-class ,widget-class)
       ,@body)))

(defmacro define-signal ((widget-class signal-name) args &body options)
  (declare (ignore options))
  `(progn
     (set-widget-class-option ',widget-class :signals '(,(qtools:specified-type-method-name signal-name args)))))

(defmacro define-subwidget ((widget-class name) initform &body body)
  (let ((initfunc (intern (format NIL "%INITIALIZE-~a-SUBWIDGET-~a" widget-class name) *package*)))
    `(progn
       (defun ,initfunc (,widget-class)
         (setf (slot-value ,widget-class ',name) ,initform)
         (qtools:with-slots-bound (,widget-class ,widget-class)
           ,@body))
       (set-widget-class-option ',widget-class :direct-slots '(:name ,name :readers NIL :writers NIL :initargs NIL))
       (set-widget-class-option ',widget-class :initializers '(,name 10 ,initfunc)))))
