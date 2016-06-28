#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

;;;;;
;; Qt Related Utils

(defgeneric value (object))

(defmethod value ((object qobject))
  (#_value object))

(defgeneric (setf value) (value object))

(defmethod (setf value) (value (object qobject))
  (#_setValue object value))

(defgeneric parent (object))

(defmethod parent ((object qobject))
  (let ((o (#_parent object)))
    (if (null-qobject-p o)
        NIL o)))

(defmethod (setf parent) (value (object qobject))
  (#_setParent object value)
  (#_show object))

(defmethod (setf parent) ((value null) (object qobject))
  (setf (parent object) (null-qobject "QWidget")))

(defun qobject-alive-p (object)
  (not (or (null-qobject-p object)
           (qobject-deleted object))))

(defun maybe-delete-qobject (object)
  (if (typep object 'abstract-qobject)
      (when (qobject-alive-p object)
        #+:verbose (v:trace :qtools "Deleting QObject: ~a" object)
        (optimized-delete object))
      #+:verbose (v:trace :qtools "Deleting QObject: WARN Tried to delete non-qobject ~a" object)))

(defun enum-equal (a b)
  (= (if (integerp a) a (qt:enum-value a))
     (if (integerp b) b (qt:enum-value b))))

(defmacro qtenumcase (keyform &body forms)
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond ,@(loop for (comp . form) in forms
                     collect (cond ((or (eql comp T)
                                        (eql comp 'otherwise))
                                    `(T ,@form))
                                   ((and (listp comp) (eql 'or (car comp)))
                                    `((or ,@(loop for c in (cdr comp) collect `(enum-equal ,key ,c))) ,@form))
                                   (T
                                    `((enum-equal ,key ,comp) ,@form))))))))

(defmacro qtypecase (instance &body cases)
  (let ((class (gensym "CLASS")))
    `(let ((,class (ensure-qclass ,instance)))
       (cond ,@(loop for (test . body) in cases
                     collect (if (find test '(T :otherwise))
                                 `(T ,@body)
                                 `((qinstancep ,class (eqt-class-name ',test)) ,@body)))))))

(defmacro qclass=case (instance &body cases)
  (let ((class (gensym "CLASS")))
    `(let ((,class (qt::qobject-class ,instance)))
       (declare (type fixnum ,class))
       (cond ,@(loop for (class . body) in cases
                     collect (if (find class '(T :otherwise))
                                 `(T ,@body)
                                 `((= ,class (the fixnum (load-time-value
                                                          (qt:find-qclass 
                                                           (eqt-class-name ',class)))))
                                   ,@body)))))))

(defun map-layout (function layout)
  (loop for i from 0
        for item = (#_itemAt layout i)
        until (null-qobject-p item)
        do (let ((widget (#_widget item))
                 (layout (#_layout item)))
             (funcall function (if (null-qobject-p widget)
                                   layout widget)))))

(defmacro do-layout ((widget layout) &body body)
  `(block NIL
     (map-layout (lambda (,widget) ,@body) ,layout)))

(defun sweep-layout (layout)
  (loop for item = (#_takeAt layout 0)
        until (typep item 'null-qobject)
        do (#_removeItem layout item)
           (finalize (#_widget item))))

(defun enumerate-method-descriptors (name args)
  (flet ((make-map (args)
           (format NIL "~a(~{~a~^, ~})" name (mapcar #'to-type-name args))))
    (cond
      ((and args (listp (first args)))
       (loop for i from 0 below (length (first args))
             collect (make-map (mapcar #'(lambda (list) (nth i list)) args))))
      (T
       (list (make-map args))))))

(defun find-children (widget child-class &key first-only)
  (let ((found ()))
    (labels ((test (widget)
               (unless (null-qobject-p widget)
                 (when (qinstancep widget child-class)
                   (if first-only
                       (return-from find-children widget)
                       (push widget found)))))
             (recurse (widget)
               (dolist (child (#_children widget))
                 (test child)
                 (recurse child))))
      (recurse widget))
    (nreverse found)))

(defun find-child (widget child-class)
  (find-children widget child-class :first-only T))

;;;;;
;; General utils

(defun ensure-qclass (thing)
  (etypecase thing
    (fixnum thing)
    (string (find-qclass thing))
    (symbol (ensure-qclass (eqt-class-name thing)))
    (qobject (qt::qobject-class thing))))

(defun ensure-class (thing)
  (etypecase thing
    (symbol (find-class thing))
    (class thing)
    (standard-object (class-of thing))))

(defmacro with-slots-bound ((instance class) &body body)
  (let ((slots (loop for slot in (c2mop:class-direct-slots
                                  (let ((class (ensure-class class)))
                                    (c2mop:finalize-inheritance class)
                                    class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

(defmacro with-all-slots-bound ((instance class) &body body)
  (let ((slots (loop for slot in (c2mop:class-slots
                                  (let ((class (ensure-class class)))
                                    (c2mop:finalize-inheritance class)
                                    class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

(defun ensure-list (a)
  (if (listp a) a (list a)))

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
                     (append (gethash option target) args))))
    (loop for key being the hash-keys of target
          for val being the hash-values of target
          collect (cons key val))))

(defun split (list items &key (key #'identity) (test #'eql))
  (loop with table = ()
        for item in list
        do (push item (getf table (find (funcall key item) items :test test)))
        finally (return (cons (nreverse (getf table NIL))
                              (loop for item in items
                                    collect (nreverse (getf table item)))))))

(defmacro with-compile-and-run (&body body)
  `(funcall
    (compile NIL `(lambda () ,,@body))))

(defun maybe-unwrap-quote (thing)
  (if (and (listp thing)
           (eql 'quote (first thing)))
      (second thing)
      thing))

(defun capitalize-on (character string &optional (replacement character) start-capitalized)
  (with-output-to-string (stream)
    (loop with capitalize = start-capitalized
          for char across (string-downcase string)
          do (cond ((char= char character)
                    (setf capitalize T)
                    (when replacement
                      (write-char replacement stream)))
                   (capitalize
                    (write-char (char-upcase char) stream)
                    (setf capitalize NIL))
                   (T
                    (write-char char stream))))))

(defmacro named-lambda (name args &body body)
  #+sbcl `(sb-int:named-lambda ,name ,args ,@body)
  #-sbcl `(lambda ,args ,@body))

(define-condition compilation-note (#+:sbcl sb-ext:compiler-note #-:sbcl condition)
  ((message :initarg :message :initform (error "MESSAGE required.") :accessor message))
  (:report (lambda (c s) (write-string (message c) s))))

(defun emit-compilation-note (format-string &rest args)
  (let ((message (apply #'format NIL format-string args)))
    #+:sbcl (sb-c:maybe-compiler-notify 'compilation-note :message message)
    #-:sbcl (signal 'compilation-note :message message)))

(defun ensure-cl-function-name (name)
  (if (and (listp name) (eql (first name) 'cl+qt:setf))
      `(cl:setf ,(second name))
      name))

(defun ensure-completable-qclass (thing)
  (etypecase thing
    (fixnum thing)
    ((or string symbol)
     (or (find-qt-class-name thing)
         (error "No corresponding Qt class found for ~a" thing)))))

(defvar *application-name* NIL)

(defun default-application-name ()
  (package-name *package*))

(defun ensure-qapplication (&key name args (main-thread T))
  (unless qt:*qapplication*
    (setf *application-name* (or name *application-name* (default-application-name)))
    (let (#+sbcl (sb-ext:*muffled-warnings* 'style-warning)
          (name *application-name*))
      (ensure-smoke :qtcore)
      (ensure-smoke :qtgui)
      (flet ((inner ()
               (let ((instance (#_QCoreApplication::instance)))
                 (setf qt:*qapplication*
                       (if (null-qobject-p instance)
                           (qt::%make-qapplication (list* name args))
                           instance))
                 (qt-libs:fix-qt-plugin-paths))))
        (if main-thread
            (tmt:call-in-main-thread #'inner :blocking T)
            (inner)))))
  qt:*qapplication*)

(defun ensure-qobject (thing)
  (etypecase thing
    (qt:qobject thing)
    (widget thing)
    (symbol (make-instance thing))))

;; Slime bug on windows. See https://common-lisp.net/project/commonqt/#known-issues
;; We just create a helper widget that immediately closes itself again.
;; That way we can execute the qapplication and ensure the weird workaround
;; is automatically performed. Hopefully it'll work fast enough on most
;; machines that the window is barely visible.
#+(and swank windows)
(progn
  (defvar *slime-fix-applied* NIL)

  (defclass slime-fix-helper ()
    ()
    (:metaclass qt:qt-class)
    (:qt-superclass "QWidget")
    (:override ("event" (lambda (this ev)
                          (declare (ignore ev))
                          (#_close this)))))

  (defmethod initialize-instance :after ((helper slime-fix-helper) &key)
    (qt:new helper))
  
  (defun fix-slime ()
    (unless *slime-fix-applied*
      (qt:with-main-window (helper (make-instance 'slime-fix-helper))
        (#_show helper)
        (#_hide helper))
      (setf *slime-fix-applied* T))))

(defmacro with-main-window ((window instantiator &key name
                                                      qapplication-args
                                                      (blocking T)
                                                      (main-thread #+darwin T #-darwin NIL)
                                                      (on-error '#'invoke-debugger)
                                                      (show T)) &body body)
  (let ((bodyfunc (gensym "BODY"))
        (innerfunc (gensym "INNER"))
        (out (gensym "OUT")))
    `(labels ((,bodyfunc ()
                (ensure-qapplication :name ,name :args ,qapplication-args :main-thread NIL)
                (handler-bind ((error ,on-error))
                  #+(and swank windows) (fix-slime)
                  (with-finalizing ((,window (ensure-qobject ,instantiator)))
                    ,@body
                    (when ,show (#_show ,window))
                    (#_exec *qapplication*))))
              (,innerfunc ()
                #+sbcl (sb-int:with-float-traps-masked (:underflow :overflow :invalid :inexact)
                         (,bodyfunc))
                #-sbcl (,bodyfunc)))
       ,(if main-thread
            `(let ((,out *standard-output*))
               (tmt:with-body-in-main-thread (:blocking ,blocking)
                 (let ((*standard-output* ,out))
                   (,innerfunc))))
            `(,innerfunc)))))
