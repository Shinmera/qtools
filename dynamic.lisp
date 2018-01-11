#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun to-readtable-case (string &optional (case (readtable-case *readtable*)))
  (ecase case
    (:upcase (string-upcase string))
    (:downcase (string-downcase string))
    (:preserve string)
    (:invert (loop with case = :dont-know
                   for char across string
                   do (cond ((upper-case-p char)
                             (case case
                               (:upcase (setf case :preserve) (loop-finish))
                               (:dont-know (setf case :downcase))))
                            ((lower-case-p char)
                             (case case
                               (:downcase (setf case :preserve) (loop-finish))
                               (:dont-know (setf case :upcase)))))
                   finally (return (to-readtable-case string case))))))

(defun ensure-q+-method (function)
  (handler-bind ((style-warning #'muffle-warning))
    (ensure-methods-processed)
    (let ((symbol (find-symbol (string function) *target-package*)))
      (cond ((not symbol)
             (error "No methods named ~s found." function))
            ((fboundp symbol))
            (T
             ;; We really do not want to
             ;;   (funcall (compile NIL `(lambda () ,(compile-wrapper symbol))))
             ;; here for the following reason: in order to allow inlining
             ;; of the information as well as direct usage of defined variables
             ;; we need to evaluate things in sequence rather than COMPILEing
             ;; them. Using EVAL here avoids the style warnings that can be
             ;; rather confusing and annoying during development and general
             ;; usage.
             (eval (compile-wrapper symbol))))
      symbol)))

(defmacro q+ (function &rest args)
  (let ((symbol (ensure-q+-method function)))
    `(progn
       (load-time-value (ensure-q+-method ',function))
       (,symbol ,@args))))

(defmacro q+fun (function)
  `(load-time-value (symbol-function (ensure-q+-method ,function))))

;;;;;
;; SETF

(defun process-q+-setter (place value)
  (when (eql (first place) 'q+)
    (setf place (rest place)))
  (let ((name (first place))
        (name-args (rest place))
        (value-args (if (and (listp value) (eql (first value) 'values))
                        (rest value)
                        (list value))))
    `(q+ ,(to-readtable-case (format NIL "SET-~a" (string name))) ,@name-args ,@value-args)))

(defmacro cl+qt:setf (&rest args)
  (assert (evenp (length args))
          () "Must supply balanced pairs of places and values.")
  `(progn
     ,@(loop for (place value) on args by #'cddr
             if (and (listp place)
                     (or (eql (first place)
                              'q+)
                         (eql (symbol-package (first place))
                              *target-package*)))
             collect (process-q+-setter place value)
             else
             collect `(cl:setf ,place ,value))))

(defmacro fsetf (&rest pairs)
  (flet ((transform (place value)
           (let* ((values (if (and (listp value) (eq (car value) 'values)) (cdr value) (list value)))
                  (gensyms (loop for v in values collect (gensym))))
             `(let ,(loop for v in values for g in gensyms collect `(,g ,v))
                (finalize ,place)
                (cl+qt:setf ,place ,(if (cdr gensyms) `(values ,@gensyms) (first gensyms)))))))
    `(progn
       ,@(loop for (place value) on pairs by #'cddr
               collect (transform place value)))))

(defmacro cl+qt:defgeneric (name args &body options)
  `(cl:defgeneric ,(ensure-cl-function-name name) ,args
     ,@options))

(defmacro cl+qt:defun (name args &body body)
  `(cl:defun ,(ensure-cl-function-name name) ,args
     ,@body))

(defun cl+qt:fdefinition (name)
  (cl:fdefinition (ensure-cl-function-name name)))

(defun (setf cl+qt:fdefinition) (function name)
  (setf (cl:fdefinition (ensure-cl-function-name name)) function))

(defmacro cl+qt:function (name)
  `(cl:function ,(ensure-cl-function-name name)))

(deftype cl+qt:function ()
  'cl:function)

;;;;;
;; Reader

(defun read-list-until (char stream &optional (recursive-p T))
  (let ((char-macro (get-macro-character char)))
    (assert char-macro)
    (loop with read
          for next-char = (peek-char T stream T NIL recursive-p)
          when (let ((macro (get-macro-character next-char)))
                 (cond ((eq char-macro macro)
                        (loop-finish))
                       ((not macro)
                        (setf read (read stream T NIL recursive-p))
                        T)
                       (T
                        (setf read
                              (multiple-value-list
                               (funcall macro stream
                                        (read-char stream T NIL recursive-p))))
                        (when read
                          (setf read (car read))
                          T))))
          collect read)))

(defun read-name (stream)
  (to-readtable-case
   (with-output-to-string (output)
     (loop for char = (peek-char NIL stream T NIL T)
           do (if (or (char= char #\Space)
                      (not (graphic-char-p char))
                      (get-macro-character char))
                  (loop-finish)
                  (write-char (read-char stream T NIL T) output))))))

(defun target-package-symbol-string (&key (extern T) (symbol ""))
  (to-readtable-case
   (format NIL "~a~:[::~;:~]~a" (package-name *target-package*) extern symbol)))

(defun q+-symbol-p (stream)
  (let ((buffer ()))
    (values
     (loop for char across (target-package-symbol-string)
        for read = (read-char stream)
        do (push read buffer)
        always (string= char (to-readtable-case (string read))))
     (make-string-input-stream (coerce (reverse buffer) 'string)))))

(defun q+-symbol-name (string)
  (cond ((string-starts-with-p (target-package-symbol-string :extern NIL) string)
         (subseq string (length (target-package-symbol-string :extern NIL))))
        ((string-starts-with-p (target-package-symbol-string :extern T) string)
         (subseq string (length (target-package-symbol-string :extern T))))
        (T (error "~s is not a ~a symbol string!" (package-name *target-package*) string))))

(defvar *standard-paren-reader* (get-macro-character #\())
(progn
  (defun read-paren (stream char)
    (multiple-value-bind (q+-symbol-p consumed-stream) (q+-symbol-p stream)
      (let ((stream (make-concatenated-stream consumed-stream stream)))
        (if q+-symbol-p
            (let* ((name (q+-symbol-name (read-name stream)))
                   (contents (read-list-until #\) stream)))
              (read-char stream) ;consume closing ).
              `(q+ ,name ,@contents))
            (funcall *standard-paren-reader* stream char)))))

  (set-macro-character #\( #'read-paren NIL (named-readtables:find-readtable :qtools)))

(defvar *standard-function-reader* (get-dispatch-macro-character #\# #\'))
(progn
  (defun read-function (stream subchar arg)
    (declare (ignore subchar arg))
    (multiple-value-bind (q+-symbol-p consumed-stream) (q+-symbol-p stream)
      (let ((stream (make-concatenated-stream consumed-stream stream)))
        (cond ;; Q+ Symbol case
          (q+-symbol-p
           (let ((name (q+-symbol-name (read-name stream))))
             `(q+fun ,name)))
          ;; Setf case
          ((eql (peek-char T stream T NIL T) #\()
           ;; Consume opening parenthesis
           (read-char stream)
           (let ((token (read stream T NIL T)))
             (cond
               ;; SETF or LAMBDA case, in which just read "as normal"
               ((or (eql token 'lambda) (eql token 'cl:setf))
                `(cl:function (,token ,@(read-delimited-list #\) stream T))))
               ;; CL+QT-setf case, in which we might have a q+ reference.
               ((eql token 'cl+qt:setf)
                ;; Consume whitespace
                (peek-char T stream T NIL T)
                (multiple-value-bind (q+-symbol-p consumed-stream) (q+-symbol-p stream)
                  (let ((stream (make-concatenated-stream consumed-stream stream)))
                    (prog1 (if q+-symbol-p
                               (let ((name (q+-symbol-name (read-name stream))))
                                 `(q+fun ,(format NIL "~a-~a" 'set name)))
                               `(cl+qt:function (cl:setf ,(read stream T NIL T))))
                      (unless (char= #\) (read-char stream NIL T T))
                        (error "A SETF function name must only contain two items."))))))
               (T
                (error "FUNCTION does not accept expressions that are not function names or LAMBDA forms.")))))
          ;; Other symbol case
          (T
           `(cl+qt:function ,(read stream T NIL T)))))))

  (set-dispatch-macro-character #\# #\' #'read-function (named-readtables:find-readtable :qtools)))
