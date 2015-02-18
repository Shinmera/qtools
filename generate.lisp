#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

;;;;;
;; Meta Processing

(defvar *methods* (make-hash-table :test 'equal))
(defvar *setters* (make-hash-table :test 'equal))
(defvar *static-methods* (make-hash-table :test 'equal))
(defvar *operators* (make-hash-table :test 'equal))
(defvar *constants* (make-hash-table :test 'equal))
(defvar *constructors* (make-hash-table :test 'equal))
(defvar *target-package* *package*)
(defvar *smoke-libs* '(:qt3support :qtcore :qtdbus
                       :qtdeclarative :qtgui :qthelp
                       :qtmultimedia :qtnetwork
                       :qtopengl :qtscript :qtsql
                       :qtsvg :qttest :qtuitools
                       :qtwebkit :qtxml :qtxmlpatterns))

(defun load-all-smoke-libs (&optional (libs *smoke-libs*))
  (dolist (lib libs)
    (ensure-smoke lib)))

(defun clear-method-info ()
  (setf *methods* (make-hash-table :test 'equal))
  (setf *setter-methods* (make-hash-table :test 'equal))
  (setf *static-methods* (make-hash-table :test 'equal))
  (setf *operators* (make-hash-table :test 'equal))
  (setf *constants* (make-hash-table :test 'equal))
  (setf *constructors* (make-hash-table :test 'equal))
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

(defun clean-method-name (method)
  (string-trim "#$?" (etypecase method
                       (integer (qmethod-name method))
                       (string method))))

(defun place-for-method (method)
  (cond ((qt::qmethod-enum-p method) *constants*)
        ((or (qt::qmethod-ctor-p method)
             (qt::qmethod-copyctor-p method)) *constructors*)
        ((qt::qmethod-dtor-p method) NIL)
        ((qt::qmethod-internal-p method) NIL)
        ((qt::qmethod-static-p method) *static-methods*)
        ;; ((qmethod-setter-p method) *setter-methods*)
        ((qmethod-cast-operator-p method) NIL)
        ((qmethod-operator-p method) *operators*)
        (T *methods*)))

(defun process-method (method)
  (let ((name (clean-method-name method))
        (place (place-for-method method)))
    (when place (push method (gethash name place)))))

(defun process-all-methods ()
  (clear-method-info)
  (qt::map-methods #'process-method))

;;;;;
;; Name Generation

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
        for char across (etypecase qmethod
                          (integer (qmethod-name qmethod))
                          (string qmethod))
        do (cond ((find char "~#$?"))
                 ((find char "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
                  (unless prev-cap
                    (write-char #\- stream))
                  (setf prev-cap T)
                  (write-char char stream))
                 ((char= char #\_)
                  (write-char #\- stream))
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
    (write-qmethod-name method stream)
    (write-char #\+ stream)))

(defun cl-variable-name (method)
  (with-output-to-target-symbol (stream)
    (write-char #\* stream)
    (write-qclass-name (qt::qmethod-class method) stream)
    (write-qmethod-name method stream)
    (write-char #\* stream)))

(defun cl-operator-name (method)
  (let ((op (subseq (qmethod-name method) 8)))
    (macrolet ((-> (&rest clauses)
                 `(cond ,@(loop for clause in clauses
                                collect (if (stringp (first clause))
                                            `((string= op ,(first clause))
                                              (target-symbol ,(second clause)))
                                            `(,@clause))))))
      (-> ("==" "=")
          ("!=" "/=")
          (">" ">")
          (">=" ">=")
          ("<" "<")
          ("<=" "<=")
          ("!" "NOT")
          ("*" "*")
          ("+" "+")
          ("-" "-")
          ("%" "MOD")
          ("~" "LOGNOT")
          ("&" "LOGAND")
          ("|" "LOGIOR")
          ("^" "LOGXOR")
          (">>" "ASH-")
          ("<<" "ASH")
          ("&&" "AND")
          ("||" "OR")
          ("[]" "AREF")
          ("()" "FUNCALL")
          ("=" "SET")
          ("+=" "INCF")
          ("-=" "DECF")
          (T NIL)))))

(defun cl-setter-name (method)
  `(setf ,(with-output-to-target-symbol (stream)
            (write-qmethod-name (subseq (qmethod-name method) 3) stream))))

(defun cl-static-method-name (method)
  (with-output-to-target-symbol (stream)
    (write-qclass-name (qt::qmethod-class method) stream)
    (write-qmethod-name method stream)))

(defun cl-method-name (method)
  (with-output-to-target-symbol (stream)
    (write-qmethod-name method stream)))

(defun %method-see (stream method &rest rest)
  (declare (ignore rest))
  (format stream "~a::~a(~{~a~^, ~})"
          (qclass-name (qt::qmethod-class method))
          (qmethod-name method)
          (mapcar #'qt::qtype-name (qt::list-qmethod-argument-types method))))

(defun generate-method-docstring (methods)
  (format NIL "Call to Qt method ~a

~{See ~/org.shirakumo.qtools::%method-see/~^~%~}"
          (clean-method-name (first methods))
          (sort (copy-list methods) #'< :key #'qt::qmethod-argument-number)))

(defun generate-constant-docstring (method)
  (format NIL "Constant for Qt enum ~a::~a"
          (qclass-name (qt::qmethod-class method))
          (clean-method-name method)))

;;;;;
;; Compilers

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
                 `(optimized-call T ,,instance ,,method ,(cddr ,whole)))))))))

(defun compile-setter (methods)
  (let ((method (clean-method-name (first methods)))
        (name (cl-setter-name (first methods)))
        (whole (target-symbol "%WHOLE"))
        (instance (target-symbol "%INSTANCE")))
    ;;; FIXME:
    ;; I don't actually know how the hell to do this right yet.
    (with-args (reqargs optargs optargs-p) methods
      NIL)))

(defun compile-static-method (class methods)
  (let ((method (clean-method-name (qmethod-name (first methods))))
        (name (cl-static-method-name (first methods)))
        (class-name (qclass-name class))
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
                 `(optimized-call T ,,class-name ,,method ,(cdr ,whole)))))))))

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
              (optimized-call T ,instance ,method)))
          ;; Special case n args
          ((string= method "operator()")
           `(progn
              (define-extern-inline-fun ,name (,instance &rest ,rest)
                ,(generate-method-docstring methods)
                (apply #'interpret-call ,instance ,method ,rest))
              (define-compiler-macro ,name (,instance &rest ,rest)
                (declare (ignore ,rest))
                `(optimized-call T ,,instance ,,method ,@,rest))))
          ;; Special case setters
          ((string= method "operator=")
           `(define-extern-macro ,name (,instance ,value)
              ,(generate-method-docstring methods)
              `(setf ,,instance (optimized-call T ,,instance ,,method ,,value))))
          ((or (string= method "operator+=")
               (string= method "operator-="))
           `(define-extern-macro ,name (,instance &optional (,value 1))
              ,(generate-method-docstring methods)
              `(setf ,,instance (optimized-call T ,,instance ,,method ,,value))))
          ;; Default case 1 arg
          (T
           `(define-extern-inline-fun ,name (,instance ,operand)
              ,(generate-method-docstring methods)
              (optimized-call T ,instance ,method ,operand))))))

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
                 `(optimized-new ,,class ,(cdr ,whole)))))))))

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

;;;;;
;; Mappers

(defun map-compile-static-methods (function methods)
  (let ((bundle (make-hash-table :test 'eq)))
    (dolist (method methods)
      (push method (gethash (qt::qmethod-class method) bundle)))
    (loop for class being the hash-keys of bundle
          for methods being the hash-values of bundle
          do (funcall function (compile-static-method class methods)))))

(defun map-compile-constants (function methods)
  (loop for method in methods
        do (funcall function (compile-constant method))))

(macrolet ((define-all-mapper (name compile-function method-table)
             (let ((function (gensym "FUNCTION"))
                   (table (gensym "TABLE"))
                   (c (gensym "NAME"))
                   (methods (gensym "METHODS")))
               `(defun ,name (,function &optional (,table ,method-table))
                  (maphash (lambda (,c ,methods)
                             (declare (ignore ,c))
                             (funcall ,function (,compile-function ,methods)))
                           ,table)))))
  (define-all-mapper map-compile-all-methods compile-method *methods*)
  (define-all-mapper map-compile-all-setters compile-setter *setters*)
  (define-all-mapper map-compile-all-operators compile-operator *operators*)
  (define-all-mapper map-compile-all-constructors compile-constructor *constructors*))

(macrolet ((define-all-mapper (name compile-function method-table)
             (let ((function (gensym "FUNCTION"))
                   (table (gensym "TABLE"))
                   (c (gensym "NAME"))
                   (methods (gensym "METHODS")))
               `(defun ,name (,function &optional (,table ,method-table))
                  (maphash (lambda (,c ,methods)
                             (declare (ignore ,c))
                             (,compile-function ,function ,methods))
                           ,table)))))
  (define-all-mapper map-compile-all-static-methods map-compile-static-methods *static-methods*)
  (define-all-mapper map-compile-all-constants map-compile-constants *constants*))

(defun map-compile-everything (function)
  (map-compile-all-methods function)
  (map-compile-all-setters function)
  (map-compile-all-static-methods function)
  (map-compile-all-operators function)
  (map-compile-all-constructors function)
  (map-compile-all-constants function))

;;;;;
;; Listers

(macrolet ((define-list-for-mapper (name mapper)
             (let ((table (gensym "TABLE"))
                   (t-p (gensym "TABLE-P"))
                   (list (gensym "LIST"))
                   (result (gensym "RESULT")))
               `(defun ,name (&optional (,table NIL ,t-p))
                  (let ((,list ()))
                    (apply
                     #',mapper #'(lambda (,result)
                                   (push ,result ,list))
                     (when ,t-p (list ,table)))
                    ,list)))))
  (define-list-for-mapper list-compile-all-methods map-compile-all-methods)
  (define-list-for-mapper list-compile-all-static-methods map-compile-all-static-methods)
  (define-list-for-mapper list-compile-all-operators map-compile-all-operators)
  (define-list-for-mapper list-compile-all-setters map-compile-all-setters)
  (define-list-for-mapper list-compile-all-constructors map-compile-all-constructors)
  (define-list-for-mapper list-compile-all-constants map-compile-all-constants))

(defun list-compile-everything ()
  (let ((list ()))
    (map-compile-everything (lambda (form) (push form list)))
    list))

;;;;;
;; Writers

(defun write-forms (mapper stream)
  (let ((i 0))
    (funcall mapper
             (lambda (form)
               (incf i)
               (when (= 0 (mod i 1000))
                 (format T "~&; On ~dth form..." i))
               (when form
                 (dolist (form (if (eql (car form) 'progn)
                                   (cdr form)
                                   (list form)))
                   (print form stream))
                 (format stream "~%"))))
    (format T "~&; ~d forms processed." i)))

(defun write-section (section mapper stream)
  (format T "~&;; Processing ~a" section)
  (format stream "~&~%;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (format stream "~&;;;; ~a~%" section)
  (write-forms mapper stream))

(defun write-all-sections (stream)
  (format T "~&;;; Writing all sections")
  (write-section "Methods" #'map-compile-all-methods stream)
  (write-section "Setters" #'map-compile-all-setters stream)
  (write-section "Static Methods" #'map-compile-all-static-methods stream)
  (write-section "Operators" #'map-compile-all-operators stream)
  (write-section "Constructors" #'map-compile-all-constructors stream)
  (write-section "Constants" #'map-compile-all-constants stream))

(defun write-everything-to-file (pathname &key (package "Q+") (if-exists :supersede) (body-processor #'write-all-sections))
  (let* ((package (cond ((typep package 'package))
                        ((find-package package) (find-package package))
                        (T (make-package package))))
         (*target-package* package)
         (*package* (find-package '#:cl-user)))
    (with-open-file (stream pathname :direction :output :if-exists if-exists)
      (format stream ";;;;; Automatically generated file to map Qt methods and enums to CL functions and constants.~%")
      (format stream ";;;;; See QTOOLS:WRITE-EVERYTHING-TO-FILE~%")
      (print `(in-package #:cl-user) stream)
      (print `(eval-when (:compile-toplevel :load-toplevel :execute)
                (unless (find-package ,(package-name package))
                  (make-package ,(package-name package)))) stream)
      (funcall body-processor stream)
      pathname)))

;; FIXME: Duplicate definitions
