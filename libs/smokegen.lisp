#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs)

(defvar *smokegen-origin* "git://anongit.kde.org/smokegen")
(defvar *smokegen-stable-version* "4.14.3")
(defvar *smokegen-stable-archive* (format NIL "http://download.kde.org/stable/~a/src/smokegen-~:*~a.tar.xz" *smokegen-stable-version*))
(defvar *smokegen-build-dir* (build-dir "smokegen"))

(defun download-smokegen (&key (version :stable)
                               (build-dir *smokegen-build-dir*)
                               (sources-dir (sources-dir build-dir)))
  (status 3 "Downloading smokegen")
  (case version
    (:stable
     (let ((archive (make-pathname :name "smokegen" :type "tar.xz" :defaults build-dir)))
       (download-file *smokegen-stable-archive* archive)
       (extract-tar-archive archive sources-dir :strip-folder T)))
    (:git
     (clone *smokegen-origin* sources-dir)))
  sources-dir)

(defun compile-smokegen (&key (build-dir *smokegen-build-dir*)
                              (sources-dir (sources-dir build-dir))
                              (compile-dir (compile-dir build-dir))
                              (package-dir *bin-dir*))
  (test-compile-prerequisites)
  (status 3 "Compiling smokegen")
  (with-chdir (compile-dir)
    (run-here "cmake ~s -DCMAKE_BUILD_TYPE=Release ~
                        -DCMAKE_INSTALL_PREFIX=~s"
              sources-dir package-dir)
    (run-here "make"))
  compile-dir)

(defun package-smokegen (&key (build-dir *smokegen-build-dir*)
                              (compile-dir (compile-dir build-dir))
                              (package-dir *bin-dir*))
  (status 3 "Packaging smokegen")
  (with-chdir (compile-dir)
    (run-here "make install"))
  (ensure-standalone-libs)
  package-dir)

(defun clean-smokegen (&key (build-dir *smokegen-build-dir*))
  (status 3 "Cleaning smokegen")
  (uiop:delete-directory-tree build-dir :validate (constantly T)))

(defun build-smokegen (&key (version :stable) (build-dir *smokegen-build-dir*) force)
  (when (and (not force) (smokegen-path))
    (error "smokegen is already installed."))
  (let* ((sources (download-smokegen :version version :build-dir build-dir))
         (compile (compile-smokegen :build-dir build-dir :sources-dir sources))
         (package (package-smokegen :build-dir build-dir :compile-dir compile)))
    (clean-smokegen)
    package))

(defun smokegen-on-path-p (path)
  (uiop:file-exists-p
   (make-pathname :name "smokegen" :defaults (relative-dir path "bin"))))

(defun smokegen-path (&key (install-dir *bin-dir*))
  (loop for dir in `(,install-dir
                     #+:unix #p"/usr"
                     #+:unix #p"/usr/local"
                     #+:windows #p"C:/Program Files/KDE/smokegen/")
        when (smokegen-on-path-p dir)
        do (return dir)))

(defun ensure-smokegen-available ()
  (or (smokegen-path)
      (build-smokegen)))
