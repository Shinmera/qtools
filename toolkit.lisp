#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

;;;;;
;; Qt Related Utils

(defgeneric value (object)
  (:documentation "Returns the VALUE of object. This usually translates to (#_value object) unless overridden."))

(defmethod value ((object qobject))
  (#_value object))

(defgeneric (setf value) (value object)
  (:documentation "Sets the VALUE of object. This usually translates to (#_setValue object) unless overridden."))

(defmethod (setf value) (value (object qobject))
  (#_setValue object value))

(defgeneric parent (object)
  (:documentation "Returns the PARENT object. This usually translates to (#_parent object) unless overridden."))

(defmethod parent ((object qobject))
  (#_parent object))

(defmethod (setf parent) (value (object qobject))
  (#_setParent object value)
  (#_show object))

(defmethod (setf parent) ((value null) (object qobject))
  (setf (parent object) (null-qobject "QWidget")))

(defun qobject-alive-p (object)
  "Returns T if the object is not null and not deleted."
  (not (or (null-qobject-p object)
           (qobject-deleted object))))

(defun maybe-delete-qobject (object)
  "Deletes the object if possible."
  (if (typep object 'abstract-qobject)
      (when (qobject-alive-p object)
        #+:verbose (v:trace :qtools "Deleting QObject: ~a" object)
        (optimized-delete object))
      #+:verbose (v:trace :qtools "Deleting QObject: WARN Tried to delete non-qobject ~a" object)))

(defmacro qtenumcase (keyform &body forms)
  "Just like CASE, but for Qt enums using QT:ENUM=."
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond ,@(loop for (comp . form) in forms
                     collect (if (or (eql comp T)
                                     (eql comp 'otherwise))
                                 `(T ,@form)
                                 `((qt:enum= ,key ,comp) ,@form)))))))

(defun map-layout (function layout)
  "Map all widgets on LAYOUT onto FUNCTION."
  (loop for i from 0
        for item = (#_itemAt layout i)
        until (typep item 'null-qobject)
        do (funcall function (#_widget item))))

(defmacro do-layout ((widget layout) &body body)
  "Iterate over all WIDGETs on LAYOUT."
  `(map-layout (lambda (,widget) ,@body) ,layout))

(defun clear-layout (layout)
  "Removes all widgets from the layout and finalizes them."
  (loop for item = (#_takeAt layout 0)
        until (typep item 'null-qobject)
        do (#_removeItem layout item)
           (finalize (#_widget item))))

(defun enumerate-method-descriptors (name args)
  "Returns a list of all possible method descriptors with NAME and ARGS.
Args may be either a list of direct types to use or a list of alternative types.
In the case of lists, the argument alternatives are taken in parallel.

Examples: 
 (.. foo '(a b)) => (\"foo(a,b)\")
 (.. foo '((a b))) => (\"foo(a)\" \"foo(b)\")
 (.. foo '((a b) (0 1))) => (\"foo(a,0)\" \"foo(b,1)\")"
  (flet ((make-map (args)
           (format NIL "~a(~{~a~^, ~})" name (mapcar #'to-type-name args))))
    (cond
      ((and args (listp (first args)))
       (loop for i from 0 below (length (first args))
             collect (make-map (mapcar #'(lambda (list) (nth i list)) args))))
      (T
       (list (make-map args))))))

;;;;;
;; General utils

(defun ensure-qclass (thing)
  (etypecase thing
    (fixnum thing)
    (string (find-qclass thing))
    (symbol (ensure-qclass (eqt-class-name thing)))))

(defun ensure-class (thing)
  "Ensures to return a CLASS.
SYMBOL -> FIND-CLASS
CLASS  -> IDENTITY
STANDARD-OBJECT -> CLASS-OF"
  (etypecase thing
    (symbol (find-class thing))
    (class thing)
    (standard-object (class-of thing))))

(defmacro with-slots-bound ((instance class) &body body)
  "Turns into a WITH-SLOTS with all direct-slots of CLASS.
Class is resolved as per ENSURE-CLASS."
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
  "Turns into a WITH-SLOTS with all slots of CLASS.
Class is resolved as per ENSURE-CLASS."
  (let ((slots (loop for slot in (c2mop:class-slots
                                  (let ((class (ensure-class class)))
                                    (c2mop:finalize-inheritance class)
                                    class))
                     for name = (c2mop:slot-definition-name slot)
                     collect name)))
    `(with-slots ,slots ,instance
       (declare (ignorable ,@slots))
       ,@body)))

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
  "Segregates items in LIST into separate lists if they mach an item in ITEMS.
The first item in the returned list is the list of unmatched items.

Example:
 (split '((0 a) (0 b) (1 a) (1 b) (2 c)) '(0 2) :key #'car)
 => '(((1 a) (1 b)) ((0 a) (0 b)) ((2 c))) "
  (loop with table = ()
        for item in list
        do (push item (getf table (find (funcall key item) items :test test)))
        finally (return (cons (nreverse (getf table NIL))
                              (loop for item in items
                                    collect (nreverse (getf table item)))))))

(defmacro with-compile-and-run (&body body)
  "Compiles BODY in a lambda and funcalls it."
  `(funcall
    (compile NIL `(lambda () ,,@body))))

(defun maybe-unwrap-quote (thing)
  "If it is a quote form, unwraps the contents. Otherwise returns it directly."
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
            ;; Speed up lookup
            (or (gethash qclass ,var)
                (setf (gethash qclass ,var)
                      (gethash (qclass-name qclass) ,var))))
           ((or symbol string)
            (gethash (ensure-completable-qclass qclass) ,var))))

       (defun (setf ,fun) (function qclass)
         (setf (gethash (ensure-completable-qclass qclass) ,var) function))

       (defun ,rem (qclass)
         (remhash (ensure-qclass qclass) ,var))

       (defmacro ,def (qclass args &body body)
         `(setf (,',fun ,qclass)
                (lambda ,args
                  ,@body)))

       (defun ,dispatcher (qclass ,@args)
         (let ((func (,fun qclass)))
           (when func (funcall func ,@args)))))))

(defvar *application-name* NIL)

(defun default-application-name ()
  "Attempts to find and return a default name to use for the application."
  (package-name *package*))

(defun ensure-qapplication (&key name args)
  "Ensures that the QT:*QAPPLICATION* is available, potentially using NAME and ARGS to initialize it.

See QT:*QAPPLICATION*
See QT:ENSURE-SMOKE"
  (unless qt:*qapplication*
    (setf *application-name* (or name *application-name* (default-application-name)))
    (let (#+sbcl (sb-ext:*muffled-warnings* 'style-warning)
          (name *application-name*))
      (ensure-smoke :qtcore)
      (ensure-smoke :qtgui)
      (tmt:with-body-in-main-thread (:blocking T)
        (let ((instance (#_QCoreApplication::instance)))
          (setf qt:*qapplication*
                (if (null-qobject-p instance)
                    (qt::%make-qapplication (list* name args))
                    instance))
          (qt-libs:set-qt-plugin-paths
           qt-libs:*standalone-libs-dir*)))))
  qt:*qapplication*)

(defun ensure-qobject (thing)
  "Makes sure that THING is a usable qobject.

If THING is a symbol, it attempts to use MAKE-INSTANCE with it."
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

(defmacro with-main-window ((window instantiator &key name qapplication-args (blocking T) (on-error '#'invoke-debugger)) &body body)
  "This is the main macro to start your application with.

It does the following:
1. Call ENSURE-QAPPLICATION with the provided NAME and QAPPLICATION-ARGS
2. Run the following in the main thread through TMT:WITH-BODY-IN-MAIN-THREAD
3. Establish a handler for ERROR that calls the ON-ERROR function if hit.
4. Bind WINDOW to the result of INSTANTIATOR, passed through ENSURE-QOBJECT
   (This means you can also just use the main window class' name)
5. Evaluate BODY
6. Call Q+:SHOW on WINDOW
7. Call Q+:EXEC on *QAPPLICATION*
   This will enter the Qt application's main loop that won't exit until your
   application terminates.
8. Upon termination, call FINALIZE on WINDOW."
  (let ((bodyfunc (gensym "BODY")))
    `(let ((out *standard-output*))
       (tmt:with-body-in-main-thread (:blocking ,blocking)
         (let ((*standard-output* out))
           (ensure-qapplication :name ,name :args ,qapplication-args)
           (flet ((,bodyfunc ()
                    (handler-bind ((error ,on-error))
                      #+(and swank windows) (fix-slime)
                      (with-finalizing ((,window (ensure-qobject ,instantiator)))
                        ,@body
                        (#_show ,window)
                        (#_exec *qapplication*)))))
             #+sbcl (sb-int:with-float-traps-masked (:underflow :overflow :invalid :inexact)
                      (,bodyfunc))
             #-sbcl (,bodyfunc)))))))
