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

(defclass qt-widget-class (finalizable-class qt-class)
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

(defgeneric process-qt-class-option (option body class)
  (:method (option body class)
    (declare (ignore class))
    `((,option ,body))))

(defmacro define-qt-class-option (option (class &rest body-lambda) &body forms)
  (assert (keywordp option) () "Option name must be a keyword.")
  (let* ((bodies (gensym "BODIES"))
         (body (gensym "BODY")))
    `(defmethod process-qt-class-option ((,(gensym "OPTION") (eql ,option)) ,bodies ,class)
       (loop for ,body in ,bodies
             collect (destructuring-bind ,body-lambda ,body
                       ,@forms)))))

(defgeneric process-qt-slot-option (option body class)
  (:method (option body class)
    (declare (ignore class))
    `((,option ,body))))

(defmacro define-qt-slot-option (option (class &rest body-lambda) &body forms)
  (assert (keywordp option) () "Option name must be a keyword.")
  (let* ((bodies (gensym "BODIES"))
         (body (gensym "BODY")))
    `(defmethod process-qt-slot-option ((,(gensym "OPTION") (eql ,option)) ,bodies ,class)
       (loop for ,body in ,bodies
             collect (destructuring-bind ,body-lambda ,body
                       ,@forms)))))

(define-qt-class-option :signal (class name args)
  `(:signals ,(mapcar #'list (enumerate-method-descriptors (to-method-name name) args))))

(define-qt-class-option :slot (class &rest body)
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
         `(lambda (,this)
            (with-slots-bound (,this ,class)
              ,@(loop for connection in connections
                      for (object func) = (cdadr connection)
                      collect `(connect! ,object ,func ,this (,name ,@(cdr func))))))))

      `(:slots ,(mapcar #'(lambda (func)
                            `(,func
                              (lambda ,clean-args
                                ,@(when doc (list doc)) ,@clean-decls
                                (with-slots-bound (,(first clean-args) ,class)
                                  ,@forms))))
                        (enumerate-method-descriptors name (mapcar #'cdr (cdr args))))))))

(define-qt-class-option :overrides (class name args &rest body)
  `(:override
    ((,(to-method-name name)
       (lambda ,args
         (with-slots-bound (,(first args) ,class)
           ,@body))))))

(define-qt-slot-option :widget (class name constructor &rest body)
  `(:direct-slots ((:name ,name :readers () :writers () :initargs () :finalized T))
    :widget ((,name ,constructor ,@body))))

(define-qt-class-option :widget (class name constructor &rest body)
  (add-initializer
   class *widget-init-priority*
   `(lambda (this)
      (with-slots-bound (this ,class)
        (setf ,name ,constructor)
        ,@body)))
  NIL)

(define-qt-class-option :layout (class name constructor &rest body)
  (add-initializer
   class *layout-init-priority*
   `(lambda (this)
      (with-slots-bound (this ,class)
        (let ((,name ,constructor))
          ,@body
          (#_setLayout this ,name)))))
  NIL)

(define-qt-class-option :initializer (class widget priority &rest body)
  (add-initializer
   class priority
   `(lambda (,widget)
      (with-slots-bound (,widget ,class)
        ,@body)))
  NIL)

;; We need to manually recreate this in order to ensure that
;; we can pass initargs that are not recognised in slots.
(defmethod make-instance ((class (eql (find-class 'qt-widget-class))) &rest initargs)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun initialize-qt-widget-class (class next args)
  (let ((args (apply #'fuse-plists
                     (loop for (option body) on args by #'cddr
                           append (process-qt-slot-option option body class)))))
    (apply #'shared-initialize class T args)
    (setf (qt-widget-initializers class) (make-array 0 :adjustable T :fill-pointer 0))
    (let ((args (apply #'fuse-plists
                       (loop for (option body) on args by #'cddr
                             append (process-qt-class-option option body class)))))
      (apply next class args))))

(defmethod initialize-instance :around ((class qt-widget-class) &rest args)
  (initialize-qt-widget-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class qt-widget-class) &rest args)
  (initialize-qt-widget-class class #'call-next-method args))

;; Superclass to further handle integration with the
;; qt-widget-class, as well as to provide a means of
;; defining general methods on all widgets.
(defclass qt-widget (finalizable)
  ()
  (:metaclass qt-widget-class))

;; We can't do with an :after method here since then it
;; would be called AFTER the user's :after method as per
;; the standard method combination order, which is too
;; late for our purposes.
(defmethod initialize-instance ((widget qt-widget) &key)
  (when (next-method-p)
    (call-next-method))
  (new widget)
  (call-initializers widget))

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
