#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qtools-example
  (:nicknames #:org.shirakumo.qtools.example)
  (:use #:cl+qt)
  (:export #:main))
(in-package #:qtools-example)
(named-readtables:in-readtable :qtools)

(define-widget repl (QTextEdit)
  ((input-begin :initform 0 :accessor input-begin)))

(define-signal (repl return-pressed) ())

(define-override (repl key-press-event) (ev)
  (cond ((or (= (q+:key ev) (enum-value (#_Qt::Key_Enter)))
             (= (q+:key ev) (enum-value (#_Qt::Key_Return))))
         (call-next-qmethod)
         (signal! repl return-pressed))
        ((= (q+:key ev) (enum-value (#_Qt::Key_Backspace)))
         (when (< (input-begin repl) (cursor repl))
           (call-next-qmethod)))
        (T
         (call-next-qmethod))))

(define-initializer (repl setup)
  (let ((font (q+:make-qfont "Monospace" 12)))
    (setf (q+:style-hint font) (#_QFont::TypeWriter))
    (setf (q+:font repl) font))
  (output-prefix repl))

(defun cursor (repl)
  (q+:position (q+:text-cursor repl)))

(defun output (repl format-string &rest args)
  (q+:move-cursor repl (#_QTextCursor::End))
  (q+:insert-html repl (apply #'format NIL format-string args))
  (q+:move-cursor repl (#_QTextCursor::End)))

(defun escape (text)
  (flet ((r (text find replace)
           (cl-ppcre:regex-replace-all find text replace)))
    (r (r (r text "&" "&amp;") "<" "&lt;") ">" "&gt;")))

(defun output-line (repl)
  (output repl "<br />"))

(defun output-form (repl form)
  (output repl "~a<br />" (escape (write-to-string form))))

(defun output-colored (repl color format-string &rest args)
  (output repl "<span style=\"color:~a;\">~a</span>" color (apply #'format NIL format-string args)))

(defun output-prefix (repl)
  (output-colored repl "red" "~a&gt;" (escape (package-name *package*)))
  (output repl "&nbsp;")
  (setf (input-begin repl) (cursor repl)))

(defun output-comment (repl format-string &rest args)
  (output-colored repl "gray" "; ~a<br />" (apply #'format NIL format-string args)))

(defun output-result (repl values)
  (if values
      (dolist (value values)
        (output-colored repl "cyan" "~a<br />" (escape (write-to-string value))))
      (output-comment repl "No values.")))

(defun output-error (repl error)
  (output-comment repl "<span style=\"color:red;\">Error:</span> ~a" (escape (princ-to-string error)))
  (output-comment repl "[Condition of type ~a]" (escape (princ-to-string (type-of error)))))

(defun input (repl)
  (assert (< (input-begin repl) (cursor repl))
          () "No input at this point.")
  (subseq (q+:to-plain-text repl) (input-begin repl) (cursor repl)))

(define-widget evaluator (QWidget)
  ())

(define-subwidget (evaluator repl) (make-instance 'repl))

(define-subwidget (evaluator layout) (q+:make-qvboxlayout evaluator)
  (setf (q+:window-title evaluator) "Evaluator")
  (q+:add-widget layout repl))

(define-slot (evaluator eval) ()
  (declare (connected repl (return-pressed)))
  (handler-case
      (let ((form (read-from-string (input repl))))
        (output-result repl (multiple-value-list (eval form))))
    (error (err)
      (output-line repl)
      (output-error repl err)))
  (output-prefix repl))

(defun main ()
  (with-main-window (window (make-instance 'evaluator))))
