#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.game)
(named-readtables:in-readtable :qtools)

(define-widget keyboard-tracker (QObject)
  ((keys :initform () :accessor keys)))

(define-slot (keyboard-tracker key-press) ((key int))
  (declare (connected *game* (key-press int)))
  (pushnew key keys))

(define-slot (keyboard-tracker key-release) ((key int))
  (declare (connected *game* (key-release int)))
  (setf keys (remove key keys)))

(defun key-p (key &optional (tracker (slot-value *game* 'keyboard-tracker)))
  (find key (keys tracker)))

(defun no-key-p (&optional (tracker (slot-value *game* 'keyboard-tracker)))
  (null (keys tracker)))

(defmacro switch-key (&rest clauses)
  `(progn
     ,@(loop for (test . body) in clauses
             for tests = (if (listp test) test `(or ,test))
             collect `(when (,(car tests)
                             ,@(loop for key in (cdr tests)
                                     collect `(key-p (q+ ,(format NIL "QT.KEY_~a" key)))))
                        ,@body))))
