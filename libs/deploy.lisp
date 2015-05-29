#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs)

(defvar *standalone-libs-dir* (ensure-directories-exist
                               (asdf:system-relative-pathname :qtools-libs "standalone" :type :directory)))

(defun ensure-standalone-libs (&key (package-dir (ensure-smokeqt-available))
                                    (standalone-dir *standalone-libs-dir*))
  (dolist (input (uiop:directory-files (relative-dir package-dir "lib")))
    (let ((output (make-pathname :directory (pathname-directory standalone-dir) :defaults input)))
      (unless (probe-file output)
        (uiop:copy-file input output))))
  (let* ((input (uiop:resolve-symlinks (cffi:foreign-library-pathname 'libcommonqt)))
         (output (make-pathname :directory (pathname-directory standalone-dir) :defaults input)))
    (unless (probe-file output)
      (uiop:copy-file input output)))
  standalone-dir)


(cffi:define-foreign-library libsmokebase
  (t (:default "libsmokebase")))

(cffi:define-foreign-library libcommonqt
  (:windows "commonqt.dll")
  (t (:default "libcommonqt")))

(defvar *libs-loaded* NIL)
(defun load-libcommonqt (&key force)
  (when (or (not *libs-loaded*) force)
    (when (asdf:find-system :qt)
      (pushnew (asdf:system-source-directory :qt) cffi:*foreign-library-directories*))
    (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
    (cffi:use-foreign-library libsmokebase)
    (cffi:use-foreign-library libcommonqt)
    (when (find-package :qt) (setf (symbol-value (find-symbol "*LIBRARY-LOADED-P*" :QT)) T))
    (setf *libs-loaded* T)))

(defun ensure-smoke (name)
  (load-libcommonqt)
  (when (find-package :qt)
    (funcall (find-symbol "ENSURE-SMOKE" :QT) name)))
