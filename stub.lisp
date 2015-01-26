(defpackage #:qtools-alternate
  (:nicknames #:org.shirakumo.qtools.alternate)
  (:use #:cl)
  (:shadow #:defmethod))
(in-package #:org.shirakumo.qtools.alternate)

(defclass widget-class (qtools:finalizable-class qt:qt-class)
  ((direct-slots :initform () :accessor widget-class-direct-slots)
   (direct-signals :initform () :accessor widget-class-direct-signals)
   (direct-override :initform () :accessor widget-class-direct-override)
   (slots :initform () :accessor widget-class-slots)
   (signals :initform () :accessor widget-class-signals)
   (override :initform () :accessor widget-class-override)))

(defclass widget (qtools:finalizable)
  ()
  (:metaclass widget-class)
  (:qt-superclass "QObject"))

(defun setup-widget-class (class next-method &rest args &key slots signals override update-widget-class &allow-other-keys)
  ;; Append extra options
  (unless update-widget-class
    (macrolet ((append! (arg accessor)
                 `(setf (getf args ,arg) (append (,accessor class) (getf args ,arg)))))
      (append! :slots widget-class-slots)
      (append! :signals widget-class-signals)
      (append! :override widget-class-override)))
  ;; Delegate
  (remf args :update-widget-class)
  (apply next-method class args)
  ;; Save directly specified options
  (unless update-widget-class
    (setf (widget-class-direct-slots class) slots)
    (setf (widget-class-direct-signals class) signals)
    (setf (widget-class-direct-override class) override)))

(cl:defmethod initialize-instance :around ((class widget-class) &rest args)
  (apply #'setup-widget-class class #'call-next-method args))

(cl:defmethod reinitialize-instance :around ((class widget-class) &rest args)
  (apply #'setup-widget-class class #'call-next-method args))

(defun update-qt-class-options (class)
  (let ((class (qtools:ensure-class class)))
    (reinitialize-instance
     class :slots (append (widget-class-slots class)
                          (widget-class-direct-slots class))
           :signals (append (widget-class-signals class)
                            (widget-class-direct-signals class))
           :override (append (widget-class-override class)
                             (widget-class-direct-override class))
           :update-widget-class T)))

(defun set-widget-class-option (class slot option)
  (let ((class (qtools:ensure-class class)))
    (let* ((options (slot-value class slot))
           (cell (find (first option) options :key #'first :test #'string=)))
      (if cell
          (when (second option)
            (setf (second cell) (second option)))
          (push option (slot-value class slot))))
    (update-qt-class-options class)))

(defun remove-widget-class-option (class slot identifier)
  (let ((class (qtools:ensure-class class)))
    (setf (slot-value class slot)
          (remove identifier (slot-value class slot) :key #'first :test #'string=))
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

;; (defmacro define-subwidget ((widget-class name) initform &body body)
;;   `(progn
;;      ))

(defmacro define-signal ((widget-class name) args &body options)
  (declare (ignore options))
  (let ((signal (qtools:specified-type-method-name name args)))
    `(progn
       (set-widget-class-option ',widget-class 'signals '(,signal))
       ',name)))

(defmacro define-slot ((widget-class method-name &optional (slot method-name)) args &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class) ,@(mapcar #'first args))
     (declare (slot ,slot ,(mapcar #'second args)))
     ,@body))

(defmacro define-override ((widget-class method-name &optional (override method-name)) args &body body)
  `(defmethod ,method-name ((,widget-class ,widget-class) ,args)
     (declare (override ,override))
     ,@body))

(defmacro defmethod (&whole whole name &rest args)
  (declare (ignore name args))
  (destructuring-bind (function name qualifiers lambda-list docstring declarations forms) (form-fiddle:split-lambda-form whole)
    (declare (ignore function))
    (let ((slot (second (find 'slot declarations :key #'caadr)))
          (override (second (find 'override declarations :key #'caadr)))
          (declarations (remove-if #'(lambda (a) (or (eql a 'slot) (eql a 'override))) declarations :key #'caadr))
          (widget-class (when (listp (first lambda-list)) (second (first lambda-list)))))
      ;; Check and translate special declarations
      (when (and (or slot override)
                 (or (not widget-class)
                     (listp widget-class)))
        (error "Missing widget-class primary specializer!"))
      (when slot
        (unless (cddr slot) (error "Slot declaration lambda-list required."))
        (setf slot (qtools:specified-type-method-name (second slot) (third slot))))
      (when override
        (setf slot (qtools:to-method-name (or (second override) name))))
      ;; Spit out
      `(progn
         (cl:defmethod ,name ,@qualifiers ,lambda-list
           ,@(when docstring (list docstring))
           ,@declarations
           ,@forms)
         ,@(when slot
             `((set-widget-class-option ',widget-class 'slots '(,slot ,name))))
         ,@(when override
             `((set-widget-class-option ',widget-class 'override '(,override ,name))))
         ',name))))
