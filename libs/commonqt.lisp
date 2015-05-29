#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defun fix-commonqt-pro-file (&key (file (asdf:system-relative-pathname :qt "commonqt.pro"))
                                   (package-dir *bin-dir*))
  (let ((contents (uiop:read-file-string file)))
    (unless (search "Qtools Fix" contents)
      (with-open-file (stream file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :error)
        (format stream "# Qtools Fix~
                        ~&LIBS += -L~s~
                        ~&INCLUDEPATH += ~s~%~%~a"
                (uiop:native-namestring (relative-dir package-dir "lib/"))
                (uiop:native-namestring (relative-dir package-dir "include/"))
                contents))
      file)))

(defun download-libcommonqt ()
  #+quicklisp
  (unless (ql-dist:installedp (ql-dist:find-system "qt"))
    (status 3 "Downloading libcommonqt")
    (ql-dist:install (ql-dist:find-system "qt")))
  #-quicklisp
  (unless (asdf:find-system :qt)
    (cerror "CommonQt is set up." "Please download commonqt and register it with ASDF."))
  (asdf:system-source-directory :qt))

(defun compile-libcommonqt (&key (sources-dir (asdf:system-source-directory :qt)))
  (test-compile-prerequisites)
  (let ((smoke-dir (ensure-smokeqt)))
    (status 3 "Compiling libcommonqt")
    (fix-commonqt-pro-file :file (make-pathname :name "commonqt" :type "pro" :defaults sources-dir)
                           :package-dir smoke-dir)
    (asdf:compile-system :qt)
    (asdf:system-source-directory :qt)))

(defun package-libcommonqt ()
  (status 3 "Packaging libcommonqt")
  (ensure-standalone-libs)
  (asdf:system-source-directory :qt))

(defun clean-libcommonqt ()
  (status 3 "Cleaning libcommonqt")
  (asdf:system-source-directory :qt))

(defun build-libcommonqt (&key force)
  (when (and (not force) (libcommonqt-path))
    (error "libcommonqt is already installed."))
  (let* ((sources (download-libcommonqt)))
    (compile-libcommonqt :sources-dir sources)
    (package-libcommonqt)
    (clean-libcommonqt)
    (libcommonqt-path)))

(defun libcommonqt-path (&key (install-dir (and (asdf:find-system :qt)
                                                (asdf:system-source-directory :qt))))
  (when install-dir
    (let ((path (uiop:file-exists-p
                 (make-pathname :name #-windows "libcommonqt"
                                      #+windows "commonqt"
                                :type #+darwin "dylib"
                                      #+windows "dll"
                                      #-(or darwin windows) "so"
                                :defaults install-dir))))
      (when path (uiop:resolve-symlinks path)))))

(defun ensure-libcommonqt ()
  (or (libcommonqt-path)
      (build-libcommonqt)))
