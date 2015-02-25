#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:qtools-evaluator
  (:nicknames #:org.shirakumo.qtools.evaluator)
  (:use #:cl+qt #:trivial-gray-streams)
  (:export #:main))
(in-package #:qtools-evaluator)
(named-readtables:in-readtable :qtools)

;; Basic REPL widget
(define-widget repl (QTextEdit)
  ((input-begin :initform 0 :accessor input-begin)))

(define-signal (repl return-pressed) ())

(define-override (repl key-press-event) (ev)
  (cond ;; Signal return pressed.
        ((or (= (q+:key ev) (q+:qt.key-enter))
             (= (q+:key ev) (q+:qt.key-return)))
         (call-next-qmethod)
         (signal! repl (return-pressed)))
        ;; Catch escape to forbid removing text before input.
        ((= (q+:key ev) (q+:qt.key-backspace))
         (when (< (input-begin repl) (cursor repl))
           (call-next-qmethod)))
        ;; Delegate standard.
        (T
         (call-next-qmethod))))

(define-initializer (repl setup)
  (let ((font (q+:make-qfont "Monospace" 12)))
    (setf (q+:style-hint font) (q+:qfont.type-writer))
    (setf (q+:font repl) font))
  (output-prefix repl))

;; Helper functions to i/o text.
(defun cursor (repl)
  (q+:position (q+:text-cursor repl)))

(defun output (repl format-string &rest args)
  (q+:move-cursor repl (q+:qtextcursor.end))
  (q+:insert-html repl (apply #'format NIL format-string args))
  (q+:move-cursor repl (q+:qtextcursor.end)))

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

(defun text (repl)
  (q+:to-plain-text repl))

(defun input (repl)
  (assert (< (input-begin repl) (cursor repl))
          () "No input at this point.")
  (subseq (text repl) (input-begin repl) (cursor repl)))

;; Minimal stream interface so that we can use *standard-output* et al. in REPL code.
(defclass repl-output-stream (fundamental-character-output-stream trivial-gray-stream-mixin)
  ((repl :initarg :repl :initform (error "REPL required.") :accessor repl)
   (buffer :initform (make-string-output-stream) :accessor buffer)))

(defmethod stream-clear-output ((stream repl-output-stream))
  (setf (buffer stream) (make-string-output-stream)))

(defmethod stream-finish-output ((stream repl-output-stream))
  (let ((string (get-output-stream-string (buffer stream))))
    (output-colored (repl stream) "orange" "~a" (cl-ppcre:regex-replace-all "\\n" (escape string) "<br />")))
  (clear-output stream))

(defmethod stream-force-output ((stream repl-output-stream))
  (stream-finish-output stream))

(defmethod stream-write-string ((stream repl-output-stream) string &optional (start 0) end)
  (write-string string (buffer stream) :start start :end end)
  (stream-finish-output stream))

(defmethod stream-write-char ((stream repl-output-stream) char)
  (write-string (string char) stream))

(defmethod stream-terpri ((stream repl-output-stream))
  (write-char #\Newline stream))

;; Main evaluator widget that ties things together.
(define-widget evaluator (QWidget)
  ((repl-output-stream :accessor repl-output-stream)))

(define-subwidget (evaluator repl) (make-instance 'repl))

(define-subwidget (evaluator layout) (q+:make-qvboxlayout evaluator)
  (setf (q+:window-title evaluator) "Evaluator")
  (q+:add-widget layout repl))

(define-initializer (evaluator setup)
  (setf (repl-output-stream evaluator)
        (make-instance 'repl-output-stream :repl repl)))

(defun eval-in-env (widget form)
  (let* ((*standard-output* (repl-output-stream widget))
         (*error-output* *standard-output*)
         (*trace-output* *standard-output*))
    (eval form)))

(define-slot (evaluator eval) ()
  (declare (connected repl (return-pressed)))
  (handler-case
      (let ((form (read-from-string (input repl))))
        (output-result repl (multiple-value-list (eval-in-env evaluator form))))
    (error (err)
      (output-line repl)
      (output-error repl err)))
  (output-prefix repl))

(defun main ()
  (with-main-window (window (make-instance 'evaluator))))
