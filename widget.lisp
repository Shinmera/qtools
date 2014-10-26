#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defclass widget-class (finalizable-class qt-class)
  ((initializers :initform (make-array 0 :adjustable T :fill-pointer 0) :accessor qt-widget-initializers)))

(defun add-initializer (class priority function)
  #+:verbose (v:debug :qtools "Adding initializer (~a ~a): ~a" class priority function)
  (let ((function (etypecase function
                    (function function)
                    (list (compile NIL function)))))
    (vector-push-extend (cons priority function) (qt-widget-initializers class))
    (setf (qt-widget-initializers class)
          (sort (qt-widget-initializers class) #'> :key #'car))))

(defun call-initializers (object)
  (loop for init across (qt-widget-initializers (ensure-class object))
        do (funcall (cdr init) object)))

(defgeneric process-widget-class-option (option body class)
  (:method (option body class)
    (declare (ignore class))
    `((,option ,body))))

(defmacro define-widget-class-option (option (class &rest body-lambda) &body forms)
  (assert (keywordp option) () "Option name must be a keyword.")
  (let* ((bodies (gensym "BODIES"))
         (body (gensym "BODY")))
    `(defmethod process-widget-class-option ((,(gensym "OPTION") (eql ,option)) ,bodies ,class)
       (loop for ,body in ,bodies
             collect (destructuring-bind ,body-lambda ,body
                       ,@forms)))))

(defgeneric process-widget-slot-option (option body class)
  (:method (option body class)
    (declare (ignore class))
    `((,option ,body))))

(defmacro define-widget-slot-option (option (class &rest body-lambda) &body forms)
  (assert (keywordp option) () "Option name must be a keyword.")
  (let* ((bodies (gensym "BODIES"))
         (body (gensym "BODY")))
    `(defmethod process-widget-slot-option ((,(gensym "OPTION") (eql ,option)) ,bodies ,class)
       (loop for ,body in ,bodies
             collect (destructuring-bind ,body-lambda ,body
                       ,@forms)))))

;; We need to manually recreate this in order to ensure that
;; we can pass initargs that are not recognised in slots.
(defmethod make-instance ((class (eql (find-class 'widget-class))) &rest initargs)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun initialize-widget-class (class next args)
  (let ((args (apply #'fuse-plists
                     (loop for (option body) on args by #'cddr
                           append (process-widget-slot-option option body class)))))
    (apply #'shared-initialize class T args)
    (setf (qt-widget-initializers class) (make-array 0 :adjustable T :fill-pointer 0))
    (let ((args (apply #'fuse-plists
                       (loop for (option body) on args by #'cddr
                             append (process-widget-class-option option body class)))))
      (apply next class args))))

(defmethod initialize-instance :around ((class widget-class) &rest args)
  (initialize-widget-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class widget-class) &rest args)
  (initialize-widget-class class #'call-next-method args))

;; Superclass to further handle integration with the
;; widget-class, as well as to provide a means of
;; defining general methods on all widgets.
(defclass widget (finalizable)
  ()
  (:metaclass widget-class))

;; We can't do with an :after method here since then it
;; would be called AFTER the user's :after method as per
;; the standard method combination order, which is too
;; late for our purposes.
(defmethod initialize-instance ((widget widget) &key)
  (when (next-method-p)
    (call-next-method))
  (new widget)
  (call-initializers widget))

(defmacro define-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  `(defclass ,name (widget ,@direct-superclasses)
     ,direct-slots
     (:metaclass widget-class)
     (:qt-superclass ,(find-qt-class-name qt-class))
     ,@(fuse-alists options)))

(indent:define-indentation define-widget
    (4 (&whole 6 &rest)
       (&whole 2 (&whole 0 0 &rest 2))
       &rest (&whole 2 2 &rest (&whole 2 2 4 &body))))
