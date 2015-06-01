#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defvar *smokeqt-origin* "git://anongit.kde.org/smokeqt")
(defvar *smokeqt-stable-version* "4.14.3")
(defvar *smokeqt-stable-archive* (format NIL "http://download.kde.org/stable/~a/src/smokeqt-~:*~a.tar.xz" *smokeqt-stable-version*))
(defvar *smokeqt-build-dir* (build-dir "smokeqt"))

(defun download-smokeqt (&key (version :stable)
                              (build-dir *smokeqt-build-dir*)
                              (sources-dir (sources-dir build-dir))
                              force)
  (when (or force (not (uiop:file-exists-p (merge-pathnames "README" sources-dir))))
    (status 3 "Downloading smokeqt")
    (case version
      (:stable
       (let ((archive (make-pathname :name "smokeqt" :type "tar.xz" :defaults build-dir)))
         (download-file *smokeqt-stable-archive* archive)
         (extract-tar-archive archive sources-dir :strip-folder T)))
      (:git
       (clone *smokeqt-origin* sources-dir))))
  sources-dir)

(defun compile-smokeqt (&key (build-dir *smokeqt-build-dir*)
                             (sources-dir (sources-dir build-dir))
                             (compile-dir (compile-dir build-dir))
                             (package-dir *bin-dir*)
                             force)
  (when (or force (not (uiop:file-exists-p (relative-dir compile-dir "bin"))))
    (test-compile-prerequisites)
    (let ((smoke-dir (ensure-smokegen)))
      (status 3 "Compiling smokeqt")
      (with-chdir (compile-dir)
        (run-here "cmake ~s -DCMAKE_BUILD_TYPE=Release ~
                            -DCMAKE_INSTALL_PREFIX=~s ~
                            -DWITH_Qwt5=OFF ~
                            -DSMOKE_INSTALL_PREFIX=~s ~
                            -Wno-dev"
                  sources-dir package-dir smoke-dir)
        (run-here "env LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH:~a:~a\" make"
                  (relative-dir smoke-dir "lib")
                  (relative-dir smoke-dir "lib" "smokegen")))))
  compile-dir)

(defun package-smokeqt (&key (build-dir *smokeqt-build-dir*)
                             (compile-dir (compile-dir build-dir))
                             (package-dir *bin-dir*)
                             force)
  (when (or force (not (uiop:file-exists-p (relative-dir package-dir "bin"))))
    (status 3 "Packaging smokeqt")
    (with-chdir (compile-dir)
      (run-here "make install")))
  package-dir)

(defun clean-smokeqt (&key (build-dir *smokeqt-build-dir*))
  (when (uiop:file-exists-p build-dir)
    (status 3 "Cleaning smokeqt")
    (uiop:delete-directory-tree build-dir :validate (constantly T))))

(defun build-smokeqt (&key (version :stable) (build-dir *smokeqt-build-dir*) force)
  (when (and (not force) (smokeqt-path))
    (error "smokeqt is already installed."))
  (let* ((sources (download-smokeqt :version version :build-dir build-dir :force force))
         (compile (compile-smokeqt :build-dir build-dir :sources-dir sources :force force))
         (package (package-smokeqt :build-dir build-dir :compile-dir compile :force force)))
    (clean-smokeqt)
    package))

(defun smokeqt-on-path-p (path)
  (uiop:file-exists-p
   (make-pathname :name "libsmokeqtcore" :type #+:linux "so" #+:darwin "dylib" #+:windows "dll"
                  :defaults (relative-dir path "lib"))))

(defun smokeqt-path (&key (install-dir *bin-dir*))
  (loop for dir in `(,install-dir
                     #+:unix #p"/usr"
                     #+:unix #p"/usr/local"
                     #+:windows #p"C:/Program Files/KDE/smokeqt/")
        when (smokeqt-on-path-p dir)
        return dir))

(defun ensure-smokeqt ()
  (or (smokeqt-path)
      (build-smokeqt)))
