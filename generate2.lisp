#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *target-package* (or (find-package "Q+")
                             (make-package "Q+")))
(defvar *smoke-modules* '(:qt3support :qtcore :qtdbus
                          :qtdeclarative :qtgui :qthelp
                          :qtmultimedia :qtnetwork
                          :qtopengl :qtscript :qtsql
                          :qtsvg :qttest :qtuitools
                          :qtwebkit :qtxml :qtxmlpatterns))
(defvar *operator-map*
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (op . name) in '(("==" . "=") ("!=" . "/=")
                               (">" . ">") (">=" . ">=")
                               ("<" . "<") ("<=" . "<=")
                               ("!" . "NOT") ("%" . "MOD")
                               ("*" . "*") ("/" . "/")
                               ("-" . "-") ("+" . "+")
                               ("~" . "LOGNOT") ("&" . "LOGAND")
                               ("|" . "LOGIOR") ("^" . "LOGXOR")
                               (">>" . "ASH-") ("<<" . "ASH")
                               ("&&" . "AND") ("||" . "OR")
                               ("[]" . "AREF") ("() ." . "FUNCALL")
                               ("+=" . "INCF") ("-=" . "DECF")
                               ("=" . "SET"))
          do (setf (gethash (format NIL "operator~a" op) table) name))
    table))
(defvar *generator-target* (asdf:system-relative-pathname :qtools "q+.lisp"))
(defvar *qmethods* (make-hash-table :test 'equal))

(defun load-all-smoke-modules (&optional (mods *smoke-modules*))
  (dolist (mod mods)
    (ensure-smoke mod)))

(defun loaded-smoke-modules ()
  (remove-if-not #'(lambda (a) (qt::named-module-number (string-downcase a)))
                 *smoke-modules*))

(defun clear-method-info ()
  (setf *qmethods* (make-hash-table :test 'equal))
  T)

(defun string-starts-with-p (start string &key (offset 0))
  (and (< (length start) (+ offset (length string)))
       (string= start string :start2 offset :end2 (+ offset (length start)))))

(defun qmethod-setter-p (method)
  (let ((name (qmethod-name method)))
    (and (string-starts-with-p "set" name)
         (upper-case-p (char name 3)))))

(defun qmethod-operator-p (method)
  (let ((name (qmethod-name method)))
    (string-starts-with-p "operator" name)))

(defun qmethod-cast-operator-p (method)
  (let ((name (qmethod-name method)))
    (string-starts-with-p "operator " name)))

(defun qmethod-bogus-p (method)
  (let ((name (qmethod-name method)))
    (string-starts-with-p "_" name)))

(defun qmethod-translatable-operator-p (method)
  (not (null (gethash (qmethod-name method) *operator-map*))))

(defun qmethod-globalspace-p (method)
  (= (qt::qmethod-class method)
     (find-qclass "QGlobalSpace")))

(defun clean-method-name (method)
  (string-trim "#$?" (etypecase method
                       (integer (qmethod-name method))
                       (string method))))

(defun target-symbol (format-string &rest format-args)
  (let ((name (apply #'format NIL format-string format-args)))
    (or (find-symbol name *target-package*)
        (intern name *target-package*))))

(defmacro with-output-to-target-symbol ((stream) &body body)
  `(intern
    (with-output-to-string (,stream)
      ,@body)
    *target-package*))

(defun write-qclass-name (qclass stream)
  (loop for char across (etypecase qclass
                          (integer (qclass-name qclass))
                          (string qclass))
        do (write-char (char-upcase char) stream)))

(defun write-qmethod-name (qmethod stream)
  (loop with prev-cap = T
        for char across (clean-method-name
                         (etypecase qmethod
                           (integer (qmethod-name qmethod))
                           (string qmethod)))
        do (cond ((find char "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                  (unless prev-cap
                    (write-char #\- stream))
                  (setf prev-cap T)
                  (write-char char stream))
                 (T (setf prev-cap NIL)
                    (write-char (char-upcase char) stream)))))

(defun cl-constructor-name (method)
  (with-output-to-target-symbol (stream)
    (write-string "MAKE-" stream)
    (write-qclass-name (qt::qmethod-class method) stream)))

(defun cl-constant-name (method)
  (with-output-to-target-symbol (stream)
    (write-char #\+ stream)
    (write-qclass-name (qt::qmethod-class method) stream)
    (write-char #\- stream)
    (write-qmethod-name method stream)
    (write-char #\+ stream)))

(defun cl-variable-name (method)
  (with-output-to-target-symbol (stream)
    (write-char #\* stream)
    (write-qclass-name (qt::qmethod-class method) stream)
    (write-char #\- stream)
    (write-qmethod-name method stream)
    (write-char #\* stream)))

(defun cl-operator-name (method)
  (let ((name (gethash (qmethod-name method) *operator-map*)))
    (if name
        (target-symbol name)
        (error "Method ~a is not a translatable operator." method))))

(defun cl-setter-name (method)
  `(setf ,(with-output-to-target-symbol (stream)
            (write-qmethod-name (subseq (qmethod-name method) 3) stream))))

(defun cl-static-method-name (method)
  (with-output-to-target-symbol (stream)
    (write-qclass-name (qt::qmethod-class method) stream)
    (write-char #\- stream)
    (write-qmethod-name method stream)))

(defun cl-method-name (method)
  (with-output-to-target-symbol (stream)
    (write-qmethod-name method stream)))

(defun method-needed-p (method)
  (and (not (qmethod-bogus-p method))
       (not (qt::qmethod-dtor-p method))
       (not (qt::qmethod-internal-p method))
       (not (qmethod-cast-operator-p method))
       (or (not (qmethod-operator-p method))
           (qmethod-translatable-operator-p method))))

(defun method-symbol (method)
  (cond
    ((qt::qmethod-enum-p method)
     (cl-constant-name method))
    ((or (qt::qmethod-ctor-p method)
         (qt::qmethod-copyctor-p method))
     (cl-constructor-name method))
    ((qmethod-operator-p method)
     (cl-operator-name method))
    ;; ((qmethod-setter-p method)
    ;;  (cl-setter-name method))
    ((qt::qmethod-static-p method)
     (cl-static-method-name method))
    (T
     (cl-method-name method))))

(defun process-method (method)
  (when (method-needed-p method)
    (push method (gethash (method-symbol method) *qmethods*))))

(defun process-all-methods ()
  (clear-method-info)
  (qt::map-methods #'process-method))

(defun ensure-methods (method)
  (or (etypecase method
        (symbol (gethash method *qmethods*))
        (list method)
        (fixnum method)
        (string (gethash (find-symbol method *target-package*) *qmethods*)))
      (error "No methods found for ~s" method)))

(defun compile-wrapper (method)
  (let* ((methods (ensure-methods method))
         (method (first methods)))
    (cond
      ((qt::qmethod-enum-p method)
       (compile-constant methods))
      ((or (qt::qmethod-ctor-p method)
           (qt::qmethod-copyctor-p method))
       (compile-constructor methods))
      ((qmethod-operator-p method)
       (compile-operator methods))
      ;; ((qmethod-setter-p method
      ;;  (compile-setter method))
      ((qt::qmethod-static-p method)
       (compile-static-method methods))
      (T
       (compile-method methods)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILERS

(defmacro with-args ((required optional optional-p) methods &body body)
  (let ((argnums (gensym "ARGNUMS"))
        (maxargs (gensym "MAXARGS"))
        (minargs (gensym "MINARGS"))
        (i (gensym "I")))
    `(let* ((,argnums (mapcar #'qt::qmethod-argument-number ,methods))
            (,maxargs (apply #'max ,argnums))
            (,minargs (apply #'min ,argnums))
            (,required (loop for ,i from 0 below ,minargs
                             collect (target-symbol "%REQ-~d" ,i)))
            (,optional (loop for ,i from 0 below (- ,maxargs ,minargs)
                             collect (target-symbol "%OPT-~d" ,i)))
            (,optional-p (loop for ,i in ,optional
                               collect `(,,i NIL ,(target-symbol "~a-P" ,i)))))
       ,@body)))

(defmacro define-extern-inline-fun (name lambda-list &body body)
  `(progn
     (export ',name ,(package-name (symbol-package name)))
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))

(defmacro define-extern-macro (name lambda-list &body body)
  `(progn
     (export ',name ,(package-name (symbol-package name)))
     (defmacro ,name ,lambda-list ,@body)))

(defun compile-method (methods)
  (let ((method (clean-method-name (first methods)))
        (name (cl-method-name (first methods)))
        (whole (target-symbol "%WHOLE"))
        (instance (target-symbol "%INSTANCE")))
    (with-args (reqargs optargs optargs-p) methods
      `(progn
         (define-extern-inline-fun ,name (,instance ,@reqargs ,@(when optargs `(&optional ,@optargs-p)))
           ,(generate-method-docstring methods)
           ,(if optargs
                `(cond
                   ,@(loop for (arg noop arg-p) in (reverse optargs-p)
                           for count from 0
                           collect `(,arg-p
                                     (optimized-call T ,instance ,method ,@reqargs ,@(butlast optargs count))))
                   (T (optimized-call T ,instance ,method ,@reqargs)))
                `(optimized-call T ,instance ,method ,@reqargs)))
         ,@(when optargs
             `((define-compiler-macro ,name (&whole ,whole ,instance ,@reqargs ,@(when optargs `(&optional ,@optargs)))
                 (declare (ignore ,@reqargs ,@optargs))
                 `(optimized-call T ,,instance ,,method ,@(cddr ,whole)))))))))

(defun compile-setter (methods)
  )

(defun compile-static-method (methods)
  (let ((class-name (qclass-name (qt::qmethod-class (first methods))))
        (method (clean-method-name (qmethod-name (first methods))))
        (name (cl-static-method-name (first methods)))
        (whole (target-symbol "%WHOLE")))
    (with-args (reqargs optargs optargs-p) methods
      `(progn
         (define-extern-inline-fun ,name (,@reqargs ,@(when optargs `(&optional ,@optargs-p)))
           ,(generate-method-docstring methods)
           ,(if optargs
                `(cond
                   ,@(loop for (arg noop arg-p) in (reverse optargs-p)
                           for count from 0
                           collect `(,arg-p
                                     (optimized-call T ,class-name ,method ,@reqargs ,@(butlast optargs count))))
                   (T (optimized-call T ,class-name ,method ,@reqargs)))
                `(optimized-call T ,class-name ,method ,@reqargs)))
         ,@(when optargs
             `((define-compiler-macro ,name (&whole ,whole ,@reqargs ,@(when optargs `(&optional ,@optargs)))
                 (declare (ignore ,@reqargs ,@optargs))
                 `(optimized-call T ,,class-name ,,method ,@(cdr ,whole)))))))))

(defun emit-operator-call (methods instance &rest args)
  (let ((method (qmethod-name (first methods)))
        (instance-class (target-symbol "%INSTANCE-CLASS"))
        (methods (remove-if (lambda (method) (string= (qclass-name (qt::qmethod-class method)) "QGlobalSpace"))
                            methods)))
    (if methods
        `(let ((,instance-class (qt::qobject-class ,instance)))
           (if (or ,@(loop for method in methods
                           for class = (qt::qmethod-class method)
                           collect `(qt:qsubclassp ,instance-class (load-time-value (find-qclass ,(qclass-name class))))))
               (optimized-call T ,instance ,method ,@args)
               (optimized-call T "QGlobalSpace" ,method ,instance ,@args)))
        `(optimized-call T "QGlobalSpace" ,method ,instance ,@args))))

(defun compile-operator (methods)
  (let ((name (cl-operator-name (first methods)))
        (method (clean-method-name (qmethod-name (first methods))))
        (rest (target-symbol "%REST"))
        (instance (target-symbol "%INSTANCE"))
        (operand (target-symbol "%OPERAND"))
        (value (target-symbol "%VALUE")))
    (cond ((not name)
           NIL)
          ;; Special case 0 args
          ((string= method "operator!")
           `(define-extern-inline-fun ,name (,instance)
              ,(generate-method-docstring methods)
              ,(emit-operator-call methods instance)))
          ;; Special case n args
          ;; Not bothering with QGlobalSpace here since it has no () op.
          ((string= method "operator()")
           `(progn
              (define-extern-inline-fun ,name (,instance &rest ,rest)
                ,(generate-method-docstring methods)
                (apply #'interpret-call ,instance ,method ,rest))
              (define-compiler-macro ,name (,instance &rest ,rest)
                `(optimized-call T ,,instance ,,method ,@,rest))))
          ;; Special case setters
          ((string= method "operator=")
           `(define-extern-macro ,name (,instance ,value)
              ,(generate-method-docstring methods)
              `(setf ,,instance ,,(emit-operator-call methods instance value))))
          ((or (string= method "operator+=")
               (string= method "operator-="))
           `(define-extern-macro ,name (,instance &optional (,value 1))
              ,(generate-method-docstring methods)
              `(setf ,,instance ,,(emit-operator-call methods instance value))))
          ;; Default case 1 arg
          (T
           `(define-extern-inline-fun ,name (,instance ,operand)
              ,(generate-method-docstring methods)
              ,(emit-operator-call methods instance operand))))))

(defun compile-constructor (methods)
  (let ((name (cl-constructor-name (first methods)))
        (class (qclass-name (qt::qmethod-class (first methods))))
        (whole (target-symbol "%WHOLE")))
    (with-args (reqargs optargs optargs-p) methods
      `(progn
         (define-extern-inline-fun ,name (,@reqargs ,@(when optargs `(&optional ,@optargs-p)))
           ,(generate-method-docstring methods)
           ,(if optargs
                `(cond
                   ,@(loop for (arg noop arg-p) in (reverse optargs-p)
                           for count from 0
                           collect `(,arg-p
                                     (optimized-new ,class ,@reqargs ,@(butlast optargs count))))
                   (T (optimized-new ,class ,@reqargs)))
                `(optimized-new ,class ,@reqargs)))
         ,@(when optargs
             `((define-compiler-macro ,name (&whole ,whole ,@reqargs ,@(when optargs `(&optional ,@optargs)))
                 (declare (ignore ,@reqargs ,@optargs))
                 `(optimized-new ,,class ,@(cdr ,whole)))))))))

(defmacro define-qt-constant (name (class method) &optional documentation)
  (let ((memovar (target-symbol "*MEMO-~a*" name)))
    `(progn
       (export ',name ,(package-name (symbol-package name)))
       (defvar ,memovar)
       (defun ,memovar ()
         (cond ((boundp ',memovar) ,memovar)
               ((find-qclass ,class)
                (setf ,memovar (optimized-call T ,class ,method)))
               (T (error ,(format NIL "Cannot fetch enum value ~a::~a. Does the class exist?"
                                  class method)))))
       (define-symbol-macro ,name (,memovar))
       ,@(when documentation
           `((setf (documentation ',name 'variable) ,documentation))))))

(defun compile-constant (method)
  (let ((constant (cl-constant-name method)))
    `(define-qt-constant ,constant
         (,(qclass-name (qt::qmethod-class method))
          ,(qmethod-name method))
         ,(generate-constant-docstring method))))
