(defpackage #:qtools-alternate
  (:nicknames #:org.shirakumo.qtools.alternate)
  (:use #:cl)
  (:shadow #:defmethod))
(in-package #:org.shirakumo.qtools.alternate)

(defclass widget-class (qtools:finalizable-class qt:qt-class)
  ((direct-options :initform () :accessor widget-class-direct-options)
   (extern-options :initform () :accessor widget-class-extern-options)))

(defclass widget (qtools:finalizable)
  ()
  (:metaclass widget-class)
  (:qt-superclass "QObject"))

(defun setup-widget-class (class next-method &rest options &key update-widget-class &allow-other-keys)
  ;; Append extra options
  (when (and (not update-widget-class)
             (slot-boundp class 'extern-options))
    (loop for (name value) on (widget-class-extern-options class) by #'cddr
          do (setf (getf options name) (append (getf options name) value))))
  ;; Delegate
  (print options)
  (remf options :update-widget-class)
  (apply next-method class options)
  ;; Save directly specified options
  (unless update-widget-class
    (setf (widget-class-direct-options class) options)))

(cl:defmethod initialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method options))

(cl:defmethod reinitialize-instance :around ((class widget-class) &rest options)
  (apply #'setup-widget-class class #'call-next-method options))

(defun update-qt-class-options (class)
  (let ((class (qtools:ensure-class class))
        ;; !! Copy list since we're destructively modifying.
        (options (copy-list (widget-class-direct-options class))))
    (loop for (name value) on (widget-class-extern-options class) by #'cddr
          do (setf (getf options name) (append (getf options name) value)))
    ;; Press new options into the class definition
    (apply #'reinitialize-instance class :update-widget-class T options)
    ;; CommonQt performs computations on finalisation
    (c2mop:finalize-inheritance class)))

(defun set-widget-class-option (class option identifier value)
  (let* ((class (qtools:ensure-class class))
         (idents (getf (widget-class-extern-options class) option)))
    (setf (getf (widget-class-extern-options class) option)
          (cons (if value
                    `(,identifier ,value)
                    `(,identifier))
                (remove identifier idents :key #'first :test #'string=)))
    (update-qt-class-options class)))

(defun remove-widget-class-option (class option identifier)
  (let ((class (qtools:ensure-class class)))
    (setf (getf (widget-class-extern-options class) option)
          (remove identifier (getf (widget-class-extern-options class) option)
                  :key #'first :test #'string=))
    (update-qt-class-options class)))

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
      `(set-widget-class-option ',(second (first lambda)) :slots ,slot ',method))))

(define-method-declaration override (&optional name)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    (let ((slot (qtools:to-method-name (or name method))))
      `(set-widget-class-option ',(second (first lambda)) :override ,slot ',name))))

(define-method-declaration initializer (priority)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    (let ((widget-class (second (first lambda))))
      `(set-widget-class-option ',widget-class :initializer '(,widget-class ,priority
                                                              (funcall (function ,method) ,widget-class))))))

(define-method-declaration finalizer (priority)
  (form-fiddle:with-destructured-lambda-form (:name method :lambda-list lambda) *method*
    (let ((widget-class (second (first lambda))))
      `(set-widget-class-option ',widget-class :finalizer '(,widget-class ,priority
                                                            (funcall (function ,method) ,widget-class))))))

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

;; (defmacro define-subwidget ((widget-class name) initform &body body)
;;   `(progn
;;      ))
