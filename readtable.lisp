#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun make-reader-for-function (function)
  #'(lambda (stream char arg)
      (declare (ignore char arg))
      `(,function ,(read stream T NIL T))))

(named-readtables:defreadtable :qtools
  (:merge :qt)
  (:dispatch-macro-char #\# #\> (make-reader-for-function 'make-gc-finalized))
  (:dispatch-macro-char #\# #\< (make-reader-for-function 'unbox)))
