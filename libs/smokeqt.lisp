#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs)

(defvar *smokeqt-origin* "git://anongit.kde.org/smokeqt")
(defvar *smokeqt-stable-version* "4.14.3")
(defvar *smokeqt-stable-archive* (format NIL "http://download.kde.org/stable/~a/src/smokeqt-~:*~a.tar.xz" *smokeqt-stable-version*))
(defvar *smokeqt-build-dir* (build-dir "smokeqt"))

(defun download-smokeqt (&key (version :stable)
                               (build-dir *smokeqt-build-dir*)
                              (sources-dir (sources-dir build-dir)))
  (status 3 "Downloading smokeqt")
  (case version
    (:stable
     (let ((archive (make-pathname :name "smokeqt" :type "tar.xz" :defaults build-dir)))
       (download-file *smokeqt-stable-archive* archive)
       (extract-tar-archive archive sources-dir :strip-folder T)))
    (:git
     (clone *smokeqt-origin* sources-dir)))
  sources-dir)

(defun compile-smokeqt (&key (build-dir *smokeqt-build-dir*)
                             (sources-dir (sources-dir build-dir))
                             (compile-dir (compile-dir build-dir))
                             (package-dir *bin-dir*))
  (test-compile-prerequisites)
  (let ((smoke-dir (ensure-smokegen-available)))
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
                (relative-dir smoke-dir "lib" "smokegen"))))
  compile-dir)

(defun package-smokeqt (&key (build-dir *smokeqt-build-dir*)
                             (compile-dir (compile-dir build-dir))
                             (package-dir *bin-dir*))
  (status 3 "Packaging smokeqt")
  (with-chdir (compile-dir)
    (run-here "make install"))
  (ensure-standalone-libs)
  package-dir)

(defun clean-smokeqt (&key (build-dir *smokeqt-build-dir*))
  (status 3 "Cleaning smokeqt")
  (uiop:delete-directory-tree build-dir :validate (constantly T)))

(defun build-smokeqt (&key (version :stable) (build-dir *smokeqt-build-dir*))
  (let* ((sources (download-smokeqt :version version :build-dir build-dir))
         (compile (compile-smokeqt :build-dir build-dir :sources-dir sources))
         (package (package-smokeqt :build-dir build-dir :compile-dir compile)))
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
        do (return dir)))

(defun ensure-smokeqt-available ()
  (or (smokeqt-path)
      (build-smokeqt)))
