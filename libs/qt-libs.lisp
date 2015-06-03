#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qt-libs
  (:nicknames #:org.shirakumo.qtools.libs)
  (:use #:cl)
  (:export
   #:*standalone-libs-dir*
   #:ensure-standalone-libs
   #:load-libcommonqt))
(in-package #:org.shirakumo.qtools.libs)

(defvar *standalone-libs-dir* (ensure-directories-exist
                               (asdf:system-relative-pathname :qt-libs "standalone" :type :directory)))

(defun determine-so-type (pathname)
  (cond ((search ".so." (pathname-name pathname))
         "so")
        (T (pathname-type pathname))))

(defun determine-so-name (pathname)
  (Cond ((search ".so." (pathname-name pathname))
         (subseq (pathname-name pathname) 0 (search ".so." (pathname-name pathname))))
        (T (pathname-name pathname))))

(defun so-file (name &optional defaults)
  (make-pathname :type #+darwin "dylib" #+windows "dll" #-(or darwin windows) "so"
                 :name name :defaults defaults))

(defun copy-libs (from to &key (test (constantly T)))
  (dolist (input (etypecase from
                   (list from)
                   (pathname (uiop:directory-files from))))
    (when (and (or (find (pathname-type input) '("dylib" "dll" "so") :test #'string=)
                   (search ".so." (pathname-name input)))
               (funcall test input))
      (let ((output (make-pathname :defaults to
                                   :type (determine-so-type input)
                                   :name (determine-so-name input))))
        (unless (uiop:file-exists-p output)
          (uiop:copy-file input output))))))

(defun ensure-standalone-libs (&key (standalone-dir *standalone-libs-dir*))
  (unless (uiop:file-exists-p (so-file "libsmokebase" standalone-dir))
    (copy-libs (qt-lib-generator:shared-library-files (asdf:find-system :smokegen)) standalone-dir
               :test (lambda (file) (search "smokebase" (pathname-name file)))))
  (unless (uiop:file-exists-p (so-file "libsmokeqtgui" standalone-dir))
    (copy-libs (qt-lib-generator:shared-library-files (asdf:find-system :smokeqt)) standalone-dir
               :test (lambda (file) (search "smokeqt" (pathname-name file)))))
  (unless (uiop:file-exists-p (so-file "commonqt" standalone-dir))
    (copy-libs (qt-lib-generator:shared-library-files (asdf:find-system :libcommonqt)) standalone-dir
               :test (lambda (file) (search "commonqt" (pathname-name file)))))
  standalone-dir)

(cffi:define-foreign-library libsmokebase
  (t (:default "libsmokebase")))

(cffi:define-foreign-library libcommonqt
  (:linux (:or "libcommonqt.so.1.0.0" "libcommonqt.so.1" "libcommonqt.so"))
  (:windows "commonqt.dll")
  (t (:default "libcommonqt")))

(defvar *libs-loaded* NIL)
(defun load-libcommonqt (&key force (ensure-libs T))
  (when (or (not *libs-loaded*) force)
    (when ensure-libs
      (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
      (asdf:load-system :libcommonqt)
      (ensure-standalone-libs))
    ;; See QT::LOAD-LIBCOMMONQT for an explanation of this.
    #+(and sbcl (not windows)) (sb-sys:enable-interrupt sb-unix:sigchld :default)
    ;; Do the loading.
    (cffi:use-foreign-library libsmokebase)
    (cffi:use-foreign-library libcommonqt)
    (when (find-package :qt) (setf (symbol-value (find-symbol "*LIBRARY-LOADED-P*" :QT)) T))
    (setf *libs-loaded* T)))
