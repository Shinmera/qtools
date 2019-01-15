#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

;;; We (ab)use fast-calls to pass around pointers that the usual
;;; CommonQt would mistakenly try to translate to strings.

(defun from-byte-array (bytes &optional (finalize T))
  (prog1 (cffi:foreign-array-to-lisp
          (fast-call (data qbytearray "void**") bytes)
          `(:array :uchar ,(fast-call (length qbytearray "int") bytes)))
    (when finalize (optimized-delete bytes))))

(defun to-byte-array (data)
  (cffi:with-pointer-to-vector-data (pointer data)
    (let ((bytes (fast-static-call (qbytearray-from-raw-data qbytearray "QByteArray" "const char*" "int")
                                   pointer (length data))))
      (fast-call (detach qbytearray) bytes)
      bytes)))
