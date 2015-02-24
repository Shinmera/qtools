#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun to-readtable-case (string &optional (readtable *readtable*))
  "Translates STRING to the proper reading case according to READTABLE.

See CL:READTABLE-CASE"
  (ecase (readtable-case readtable)
    (:upcase (string-upcase string))
    (:downcase (string-downcase string))
    (:preserve string)
    (:invert (with-output-to-string (stream)
               (loop for char across string
                     do (cond ((upper-case-p char) (write-char (char-downcase char) stream))
                              ((lower-case-p char) (write-char (char-upcase char) stream))
                              (T (write-char char stream))))))))

(defun ensure-q+-method (function)
  "Ensures that the Q+ FUNCTION exists by compiling it on the fly.
Raises an error if no appropriate function can be found.
Returns the proper *TARGET-PACKAGE* symbol for the function.

See QTOOLS:ENSURE-METHODS-PROCESSED
See QTOOLS:COMPILE-WRAPPER
See QTOOLS:*TARGET-PACKAGE*"
  (handler-bind ((style-warning #'muffle-warning))
    (ensure-methods-processed)
    (let ((symbol (find-symbol (string function) *target-package*)))
      (cond ((not symbol)
             (error "No methods named ~s found." function))
            ((fboundp symbol))
            (T
             (funcall
              (compile NIL `(lambda () ,(compile-wrapper symbol))))))
      symbol)))

(defmacro q+ (function &rest args)
  "Emits a function call to the Q+ FUNCTION with ARGS.

This macro does a bit of a complicated thing:
Firstly, it calls ENSURE-Q+-METHOD on FUNCTION to
make sure that the function object exists at compile
time. Then it emits a PROGN form that contains two
forms, the first of which is a LOAD-TIME-VALUE form
with a call to ENSURE-Q+-METHOD again. This is required
since the function compiled by ENSURE-Q+-METHOD is not
dumped to file anywhere and thus must be recreated at
load time to be available. The second form in the PROGN
is the actual function call, using the proper symbol
from the *TARGET-PACKAGE*.

See QTOOLS:ENSURE-Q+-METHOD"
  (let ((symbol (ensure-q+-method function)))
    `(progn
       (load-time-value (ensure-q+-method ',function))
       (,symbol ,@args))))

(defmacro q+fun (function)
  "Emits a form that evaluates to the function object of FUNCTION.

Specifically, it returns a LOAD-TIME-VALUE form that evaluates to
the function object, while ensuring that the function does indeed
exist.

See QTOOLS:ENSURE-Q+-METHOD"
  `(load-time-value (symbol-function (ensure-q+-method ,function))))

;;;;;
;; SETF

(defun process-q+-setter (place value)
  "Processes a PLACE and VALUE pair for a Q+ setter call.
PLACE should be a form calling the Q+ macro, or a form calling
a symbol in the *TARGET-PACKAGE* directly. The name of the
function being called is prefixed with \"SET-\", and is then
used to form the function name for the resulting Q+ call.
If the VALUE is a VALUES form, then all the parts of values
are used as individual arguments in the resulting Q+ call.

Example: (process-q+-setter '(q+ foo 0 1) '(values 2 3))
=> (q+ \"FOO\" 0 1 2 3)

See QTOOLS:Q+
See CL+QT:SETF
See QTOOLS:*TARGET-PACKAGE*"
  (when (eql (first place) 'q+)
    (setf place (rest place)))
  (let ((name (first place))
        (name-args (rest place))
        (value-args (if (and (listp value) (eql (first value) 'values))
                        (rest value)
                        (list value))))
    `(q+ ,(to-readtable-case (format NIL "SET-~a" (string name))) ,@name-args ,@value-args)))

(defmacro cl+qt:setf (&rest args)
  "A wrapper around CL:SETF that specially handles calls to Q+ functions.

If a place is a Q+ form, or a form calling a symbol from *TARGET-PACKAGE*,
it is translated to a proper setter call using PROCESS-Q+-SETTER. Any other
place and value pair is translated to a normal CL:SETF call.
The order of places and values is preserved.

See QTOOLS:Q+
See QTOOLS:PROCESS-Q+-SETTER
See QTOOLS:*TARGET-PACKAGE*"
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

;;;;;
;; Reader

(defun read-list-until (char stream &optional (recursive-p T))
  "Reads from STREAM using READ or an appropriate macro character until CHAR is encountered.
Returns a list of results from the calls to the various reader functions."
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
  "Attempts to read a name from STREAM.
This basically just reads ahead until it encounters either a
GRAPHIC-CHAR, a #\SPACE or a MACRO-CHARACTER. The resulting
string is properly transformed according to TO-READTABLE-CASE.

See QTOOLS:TO-READTABLE-CASE"
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
  "Returns T if the next thing on the STREAM is a Q+ symbol from *TARGET-PACKAGE*.

KLUDGE: This takes into account that UNREAD-CHAR can be
called multiple times consecutively, which is not allowed
as per the hyperspec. An alternative solution to this 
problem should be found as it will break on various
conforming implementations."
  (let ((buffer ()))
    (prog1
        (loop for char across (target-package-symbol-string)
              for read = (read-char stream)
              do (push read buffer)
              always (string= char (to-readtable-case (string read))))
      (dolist (char buffer)
        (unread-char char stream)))))

(defun q+-symbol-name (string)
  "Returns the symbol name of a *TARGET-PACKAGE* symbol in printed, string form.

See *TARGET-PACKAGE*"
  (cond ((string-starts-with-p (target-package-symbol-string :extern NIL) string)
         (subseq string (length (target-package-symbol-string :extern NIL))))
        ((string-starts-with-p (target-package-symbol-string :extern T) string)
         (subseq string (length (target-package-symbol-string :extern T))))
        (T (error "~s is not a ~a symbol string!" (package-name *target-package*) string))))

(defvar *standard-paren-reader* (get-macro-character #\()
  "Contains the macro character associated with the opening paren in the standard readtable.")
(progn
  (defun read-paren (stream char)
    "Special paren reader macro that dispatches if a call to a Q+ function is noticed.
If such a symbol is noticed, it instead emits a call to the Q+ macro. Otherwise it
dispatches to the standard opening paren macro character.

See QTOOLS:Q+-SYMBOL-P
See QTOOLS:Q+-SYMBOL-NAME
See QTOOLS:Q+
See QTOOLS:*STANDARD-PAREN-READER*"
    (if (q+-symbol-p stream)
        (let* ((name (q+-symbol-name (read-name stream)))
               (contents (read-list-until #\) stream)))
          (read-char stream) ;consume closing ).
          `(q+ ,name ,@contents))
        (funcall *standard-paren-reader* stream char)))

  (set-macro-character #\( #'read-paren NIL (named-readtables:find-readtable :qtools)))

(defvar *standard-function-reader* (get-dispatch-macro-character #\# #\')
  "Contains the dispatch macro character associated with the #' function in the standard readtable.")
(progn
  (defun read-function (stream subchar arg)
    "Special #' reader macro that dispatches if a reference to a Q+ function is noticed.
If such a symbol is noticed, it instead emits a call to the Q+FUN"
    (if (q+-symbol-p stream)
        (let ((name (q+-symbol-name (read-name stream))))
          `(q+fun ,name))
        (funcall *standard-function-reader* stream subchar arg)))

  (set-dispatch-macro-character #\# #\' #'read-function (named-readtables:find-readtable :qtools)))
