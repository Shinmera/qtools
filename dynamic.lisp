#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun ensure-q+-method (symbol)
  (handler-bind ((style-warning #'muffle-warning))
    (let ((symbol (find-symbol (symbol-name symbol) "Q+")))
      (unless (and symbol (fboundp symbol))
        (ensure-methods-processed)
        (funcall
         (compile NIL `(lambda () ,(compile-wrapper symbol)))))))
  NIL)

(defmacro with-q+-method-call (symbol &rest args)
  (ensure-q+-method symbol)
  `(progn
     (load-time-value (ensure-q+-method ',symbol))
     (,symbol ,@args)))

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
  (with-output-to-string (output)
    (loop for char = (peek-char NIL stream T NIL T)
          do (if (or (char= char #\Space)
                     (not (graphic-char-p char))
                     (get-macro-character char))
                 (loop-finish)
                 (write-char (read-char stream T NIL T) output)))))

(defun q+-symbol-p (stream)
  (let ((buffer ()))
    (prog1
        ;; FIXME: readtable-case 
        (loop for char across "q+:"
              for read = (read-char stream)
              do (push read buffer)
              always (char-equal char read))
      (dolist (char buffer)
        (unread-char char stream)))))

(defun q+-symbol-name (string)
  (cond ((string-starts-with-p "q+::" string)
         (subseq string (length "q+::")))
        ((string-starts-with-p "q+:" string)
         (subseq string (length "q+:")))
        (T (error "~s is not a Q+ symbol string!" string))))

(defvar *standard-paren-reader* (get-macro-character #\())
(progn
  (defun read-paren (stream char)
    (if (q+-symbol-p stream)
        (let* ((name (q+-symbol-name (read-name stream)))
               (symbol (let ((*package* (find-package :q+)))
                         (read-from-string name)))
               (contents (read-list-until #\) stream)))
          (read-char stream) ;consume closing ).
          `(with-q+-method-call ,symbol ,@contents))
        (funcall *standard-paren-reader* stream char)))

  (set-macro-character #\( #'read-paren NIL (named-readtables:find-readtable :qtools)))
