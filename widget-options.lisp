#|
This file is a part of Qtools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *widget-init-priority* 10)
(defvar *slot-init-priority* 20)
(defvar *layout-init-priority* 30)

(define-widget-class-option :defsignals (class name args &rest decls)
  "Allows a more handy definition of Qt signals. 
See the CommonQt :signals class option.

Expected is a list of the following structure:

BODY        ::= NAME (ARG*) DECLARATION*
ARG         ::= QT-TYPE | (QT-TYPE*)
DECLARATION ::= (declare (method [NAME]))

A METHOD declaration has the effect of generating
signalling methods for the signal. See
DEFIEN-SIGNAL-METHOD."
  (declare (ignore class))
  (destructuring-bind (declarations methods) (split decls '(method) :key #'caadr :test #'string=)
    (declare (ignore declarations))
    (when methods
      (assert (= 1 (length methods)) () "Only one method declaration is allowed.")
      (let ((method-name (or (cadadr (first methods))
                             (intern (format NIL "SIGNAL-~a" name)))))
        (with-compile-and-run
          `(define-signal-method (,name ,method-name) ,args)))))
  `(:signals ,(mapcar #'list (enumerate-method-descriptors (to-method-name name) args))))

(define-widget-class-option :defslots (class &rest body)
  "Allows a more handy definition of Qt slots.
See the CommonQt :slots class option.

Expected is a list of the following structure:

BODY        ::= NAME (ARG*) [[DOCSTRING | DECLARATION*]] FORM*
ARG         ::= SYMBOL | (SYMBOL QT-TYPE*)
DECLARATION ::= CL-DECLARATION
                | (declare (connected OBJECT FUNCTION))
                | (declare (method [NAME])

A CONNECTED declaration has the effect of automatically
executing a CONNECT! statement on instantiation.

A METHOD declaration has the effect of generating corresponding
methods instead of lambda-bodies for the slot function.

The body of the function is automatically wrapped in
WITH-SLOTS-BOUND, making all direct class-slots bound to symbols
of the slot-name."
  ;; Split apart our lambda
  (form-fiddle:with-destructured-lambda-form (:name name :lambda-list args :docstring doc :declarations decls :forms forms) (cons :slot body)
    ;; Filter declarations
    (destructuring-bind (declarations connections methods) (split decls '(connected method) :key #'caadr :test #'string=)
      ;; Build pure forms
      (let* ((clean-args (mapcar #'(lambda (a) (if (listp a) (car a) a)) args))
             (this (first clean-args))
             (cpp-name (to-method-name name))
             (cpp-args (mapcar #'cdr (cdr args)))
             (func-body `(,@(when doc (list doc))
                          ,@declarations
                          (with-slots-bound (,(first clean-args) ,class)
                            ,@forms))))
        ;; Add our connectors
        (when connections
          (add-initializer
           class *slot-init-priority*
           `(lambda (,this)
              (with-slots-bound (,this ,class)
                ,@(loop for connection in connections
                        for (object func) = (cdadr connection)
                        collect `(connect! ,object ,func ,this (,cpp-name ,@(cdr func))))))))
        ;; Define our methods
        (when methods
          (assert (= 1 (length methods)) () "Only one method declaration is allowed.")
          ;; Exchange name of method if specified
          (setf name (or (cadadr (first methods)) name))
          ;; Compile the methods now.
          (flet ((compile-method (&rest types)
                   (with-compile-and-run
                     `(defmethod ,name ((,this ,(class-name class))
                                        ,@(loop for type in types
                                                for var in (cdr clean-args)
                                                collect `(,var ,(ecl-type-for type))))
                        ,@func-body))))
            (apply #'map NIL #'compile-method cpp-args)))
        ;; Sputter out the slots
        (flet ((build-slot (func)
                 `(,func ,(if methods name `(lambda ,clean-args ,@func-body)))))
          `(:slots ,(mapcar #'build-slot (enumerate-method-descriptors cpp-name cpp-args))))))))

(define-widget-class-option :defoverrides (class name args &rest body)
  "Allows a more handy definition of override methods.
See the CommonQt :overrides class slot option.

Expected is a list of the following structure:
BODY ::= NAME ARGS FORM*

The body of the function is automatically wrapped in
WITH-SLOTS-BOUND, making all direct class-slots bound to symbols
of the slot-name."
  `(:override
    ((,(to-method-name name)
      (lambda ,args
        (with-slots-bound (,(first args) ,class)
          ,@body))))))

(define-widget-slot-option :subwidget (class name constructor &rest body)
  "Defines a slot on the class to store the subwidget. The slot has no readers or writers and is FINALIZED."
  (declare (ignore class))
  `(:direct-slots ((:name ,name :readers () :writers () :initargs () :finalized T))
    :subwidget ((,name ,constructor ,@body))))

(define-widget-class-option :subwidget (class name constructor &rest body)
  "Defines a sub-widget on the widget.

Expected is a list of the following structure:
BODY ::= NAME CONSTRUCTOR-FORM FORM*

When the class is instantiated, this form is evaluated in
the following manner: All class-slots are bound as per 
WITH-SLOTS-BOUND and the corresponding sub-widget
slot is set to the resulting value of CONSTRUCTOR-FORM.
Then the remaining body-forms are evaluated. These should
mainly be used to set all the initial properties of the widget,
to set up signal connections, etc.

The main widget is bound to the symbol QTOOLS:WIDGET."
  (add-initializer
   class *widget-init-priority*
   `(lambda (widget)
      (with-simple-restart (:skip ,(format NIL "Skip the ~s subwidget setup (Not recommended.)" name))
        (with-slots-bound (widget ,class)
          (setf ,name ,constructor)
          ,@body))))
  NIL)

(define-widget-class-option :layout (class name constructor &rest body)
  "Shorthand macro to define the widget's layout.

Expected is a list of the following structure:
BODY ::= NAME CONSTRUCTOR-FORM FORM*

When the class is instantiated and all sub-widgets have been
initialised, this form is evaluated in the following manner:
The slots are bound as per WITH-SLOTS-BOUND, the NAME symbol
is bound to the value of the CONSTRUCTOR-FORM. The FORMs are
evaluated and then #_setLayout is executed using NAME.

The main widget is bound to the symbol QTOOLS:WIDGET"
  (add-initializer
   class *layout-init-priority*
   `(lambda (widget)
      (with-simple-restart (:skip ,(format NIL "Skip the ~s layout setup (Not recommended.)" name))
        (with-slots-bound (widget ,class)
          (let ((,name ,constructor))
            ,@body
            (#_setLayout widget ,name))))))
  NIL)

(define-widget-class-option :initializer (class widget priority &rest body)
  "Defines a function to run as an initializer.
This is useful in order to interject things to be evaluated
before or between the initialization steps of a widget class.

Expected is a list of the following structure:
BODY ::= WIDGET PRIORITY FORM*

The main widget is bound to the WIDGET symbol and all its slots
are bound as per WITH-SLOTS-BOUND. The initializer is evaluated
in sequence according to its PRIORITY. The higher the value, the
later in the process it is executed.

See *WIDGET-INIT-PRIORITY*
See *SLOT-INIT-PRIORITY*
See *LAYOUT-INIT-PRIORITY*"
  (add-initializer
   class priority
   `(lambda (,widget)
      (with-simple-restart (:skip "Skip the initializer.")
        (with-slots-bound (,widget ,class)
          ,@body))))
  NIL)

(define-widget-class-option :finalizer (class widget priority &rest body)
  "Defines a function to run as a finalizer.
This is useful in order to interject things to be evaluated
before the finalization of a widget class.

Expected is a list of the following structure:
BODY ::= WIDGET PRIORITY FORM*

The main widget is bound to the WIDGET symbol and all its slots
are bound as per WITH-SLOTS-BOUND. The finalizer is evaluated
in sequence according to its PRIORITY. The higher the value, the
later in the process it is executed."
  (add-finalizer
   class priority
   `(lambda (,widget)
      (with-simple-restart (:skip "Skip the finalizer.")
        (with-slots-bound (,widget ,class)
          ,@body))))
  NIL)
