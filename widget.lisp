#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defclass widget-class (finalizable-class qt-class)
  ((direct-options :initform () :accessor widget-class-direct-options)
   (extern-options :initform () :accessor widget-class-extern-options)
   (direct-initializers :initform () :initarg :initializers :accessor widget-class-direct-initializers)
   (direct-finalizers :initform () :initarg :finalizers :accessor widget-class-direct-finalizers)
   (initializers :initform () :accessor widget-class-initializers)
   (finalizers :initform () :accessor widget-class-finalizers)))

(defclass widget (finalizable)
  ()
  (:metaclass widget-class)
  (:qt-superclass "QObject"))

(defun call-initializers (class)
  (mapc #'(lambda (function) (funcall (third function) class))
        (widget-class-initializers (ensure-class class)))
  class)

(defun call-finalizers (class)
  (mapc #'(lambda (function) (funcall (third function) class))
        (widget-class-finalizers (ensure-class class)))
  class)

(defgeneric construct (widget))

(defmethod construct ((widget widget))
  (new widget))

(defmethod initialize-instance ((widget widget) &key)
  (when (next-method-p)
    (call-next-method))
  (construct widget)
  (call-initializers widget)
  widget)

(defmethod finalize :before ((widget widget))
  (call-finalizers widget))

(define-condition invalid-qt-superclass-hierarchy (error)
  ((requested-qt-superclass :initarg :requested-qt-superclass :accessor requested-qt-superclass)
   (clashing-qt-superclass :initarg :clashing-qt-superclass :accessor clashing-qt-superclass)
   (clashing-superclass :initarg :clashing-superclass :accessor clashing-superclass))
  (:report (lambda (c s) (format s "Cannot use ~a as Qt-superclass, ~&because it is not a Qt-subclass of ~a which is a ~&transitive Qt-superclass of the ~a direct-superclass."
                                 (qclass-name (requested-qt-superclass c)) (qclass-name (clashing-qt-superclass c)) (class-name (clashing-superclass c))))))

(defun check-qt-superclass-compatibility (qt-superclass direct-superclasses)
  (let* ((qt-superclass (ensure-qclass qt-superclass))
         (class-precedence (qclass-precedence-list qt-superclass)))
    (labels ((check (class direct)
               (let ((class (ignore-errors (ensure-class class))))
                 (when (and class (c2mop:subclassp class (find-class 'widget)))
                   (let ((qt-class (ensure-qclass (qt::class-qt-superclass class))))
                     (unless (find qt-class class-precedence)
                       (error 'invalid-qt-superclass-hierarchy
                              :requested-qt-superclass qt-superclass
                              :clashing-qt-superclass qt-class
                              :clashing-superclass class)))
                   (dolist (class (c2mop:class-direct-superclasses class))
                     (check class direct))))))
      (dolist (class direct-superclasses)
        (check class class)))))

(defun setup-widget-class (class next-method &rest options &key direct-superclasses qt-superclass initializers finalizers (save-direct-options T) (constructor () c-p) &allow-other-keys)
  (declare (ignore initializers finalizers))
  (check-qt-superclass-compatibility (first qt-superclass) direct-superclasses)
  (let ((original-options (copy-list options)))
    ;; Append extra options
    (when (slot-boundp class 'extern-options)
      (loop for (name value) on (widget-class-extern-options class) by #'cddr
            do (setf (getf options name) (append (getf options name) value))))
    ;; Delegate
    (remf options :save-direct-options)
    #+:verbose (unless (getf options 'init) (v:debug :qtools.widget "~s Delegating class options: ~s" class options))
    (remf options 'init)
    (apply next-method class options)
    ;; Save directly specified options
    (when save-direct-options
      (setf (widget-class-direct-options class)
            original-options)))
  ;; Compile constructor
  (when c-p
    (setf (find-class (class-name class)) class)
    (let ((instance (gensym "INSTANCE")))
      (funcall
       (compile NIL `(lambda NIL
                       (defmethod construct ((,instance ,(class-name class)))
                         (with-all-slots-bound (,instance ,(class-name class))
                           (new ,instance ,@constructor))))))))
  class)

(defmethod initialize-instance :around ((class widget-class) &rest options &key constructor &allow-other-keys)
  (declare (ignore constructor))
  (apply #'setup-widget-class class #'call-next-method 'init T options))

(defmethod reinitialize-instance :around ((class widget-class) &rest options &key constructor &allow-other-keys)
  (declare (ignore constructor))
  (apply #'setup-widget-class class #'call-next-method options))

(defun set-effective-option (class slot direct-values &key (direct-superclasses (c2mop:class-direct-superclasses class)))
  (let ((values (copy-list direct-values)))
    (dolist (superclass direct-superclasses)
      (when (c2mop:subclassp superclass 'widget)
        (dolist (value (reverse (slot-value superclass slot)))
          (pushnew value values :key #'third))))
    (setf (slot-value class slot)
          (stable-sort values #'> :key #'second))))

(defun cascade-option-changes (class)
  (set-effective-option class 'initializers (widget-class-direct-initializers class))
  (set-effective-option class 'finalizers (widget-class-direct-finalizers class))
  (loop for sub-class in (c2mop:class-direct-subclasses class)
        when (and (c2mop:subclassp sub-class 'widget-class)
                  (c2mop:class-finalized-p sub-class))
        do (cascade-option-changes sub-class)))

(defmethod c2mop:finalize-inheritance :after ((class widget-class))
  (dolist (super (c2mop:class-direct-superclasses class))
    (unless (c2mop:class-finalized-p super)
      (c2mop:finalize-inheritance super)))
  (cascade-option-changes class))

(defun softly-redefine-widget-class (class)
  (let ((class (ensure-class class)))
    #+:verbose (v:debug :qtools.widget "Softly redefining widget class ~s" class)
    ;; Press new options into the class definition
    (apply #'reinitialize-instance class :save-direct-options NIL (copy-list (widget-class-direct-options class)))
    ;; CommonQt performs computations on finalisation
    (c2mop:finalize-inheritance class)
    class))

(defun widget-class-option-p (class option value &key (key #'first) (test #'equal))
  (let ((idents (getf (widget-class-extern-options (ensure-class class)) option)))
    (find (funcall key value) idents :key key :test test)))

(defun set-widget-class-option (class option value &key (key #'first) (test #'equal))
  (let* ((identifier (funcall key value))
         (class (ensure-class class))
         (idents (getf (widget-class-extern-options class) option)))
    (cond ((not idents)
           (setf (getf (widget-class-extern-options class) option)
                 (list value)))
          ((find identifier idents :key key :test test)
           (setf (nth (position identifier idents :key key :test test) idents)
                 value))
          (T
           (setf (cdr (last idents))
                 (list value))))
    (softly-redefine-widget-class class)))

(defun remove-widget-class-option (class option identifier &key (key #'first) (test #'equal))
  (let ((class (ensure-class class)))
    (setf (getf (widget-class-extern-options class) option)
          (remove identifier (getf (widget-class-extern-options class) option)
                  :key key :test test))
    (softly-redefine-widget-class class)))

(defmacro define-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'widget)))
    (push 'widget direct-superclasses))
  (pushnew `(:metaclass widget-class) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  (pushnew `(:qt-superclass ,(eqt-class-name qt-class)) options
           :test #'(lambda (a b) (eql (car a) (car b))))
  ;; Ensure compile-time test
  (check-qt-superclass-compatibility qt-class direct-superclasses)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)))

(indent:define-indentation define-widget
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

(defmacro define-object (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  `(define-widget ,name (,qt-class ,@direct-superclasses) ,direct-slots ,@options))

(indent:define-indentation define-object
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))

;; Widgets are not qobject instances, but they often exhibit
;; a value method, so provide a default here.
(defmethod value ((widget widget))
  (#_value widget))

(defmethod (setf value) (value (widget widget))
  (#_setValue widget value))
