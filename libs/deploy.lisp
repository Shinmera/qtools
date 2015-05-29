#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defvar *standalone-libs-dir* (ensure-directories-exist
                               (asdf:system-relative-pathname :qtools-lib-generator "standalone" :type :directory)))

(defun copy-libs (from to &key (test (constantly T)))
  (dolist (input (uiop:directory-files from))
    (when (and (or (find (pathname-type from) '("dylib" "dll" "so") :test #'string=)
                   (search ".so." (pathname-name input)))
               (funcall test input))
      (let ((output (make-pathname :defaults to
                                   :type (determine-so-type input)
                                   :name (determine-so-name input))))
        (unless (uiop:file-exists-p output)
          (uiop:copy-file input output))))))

(defun ensure-standalone-libs (&key (smokegen-dir (smokegen-path))
                                    (smokeqt-dir (smokeqt-path))
                                    (commonqt-dir (libcommonqt-path))
                                    (standalone-dir *standalone-libs-dir*))
  (unless (uiop:file-exists-p (so-file "libsmokebase" standalone-dir))
    (copy-libs (relative-dir smokegen-dir "lib") standalone-dir
               :test (lambda (file) (search "smokebase" (pathname-name file)))))
  (unless (uiop:file-exists-p (so-file "libsmokeqtgui" standalone-dir))
    (copy-libs (relative-dir smokeqt-dir "lib") standalone-dir
               :test (lambda (file) (search "smokeqt" (pathname-name file)))))
  (unless (uiop:file-exists-p (so-file "libcommonqt" standalone-dir))
    (copy-libs (uiop:pathname-directory-pathname commonqt-dir) standalone-dir
               :test (lambda (file) (search "commonqt" (pathname-name file)))))
  standalone-dir)

(cffi:define-foreign-library libsmokebase
  (t (:default "libsmokebase")))

(cffi:define-foreign-library libcommonqt
  (:linux (:or "libcommonqt.so.1.0.0" "libcommonqt.so.1" "libcommonqt.so"))
  (:windows "commonqt.dll")
  (t (:default "libcommonqt")))

(defvar *libs-loaded* NIL)
(defun load-libcommonqt (&key force)
  (when (or (not *libs-loaded*) force)
    (pushnew *standalone-libs-dir* cffi:*foreign-library-directories*)
    (ensure-libcommonqt)
    (ensure-standalone-libs)
    (cffi:use-foreign-library libsmokebase)
    (cffi:use-foreign-library libcommonqt)
    (when (find-package :qt) (setf (symbol-value (find-symbol "*LIBRARY-LOADED-P*" :QT)) T))
    (setf *libs-loaded* T)))
