#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokeqt
  :class build-system
  :version "4.14.3"
  :depends-on (:qt-build-prerequisites
               :smokegen))

(defmethod origin ((system (eql (asdf:find-system :smokeqt))))
  (let ((version (asdf:component-version system)))
    (if (eql version :git)
        "git://anongit.kde.org/smokeqt"
        (format NIL "http://download.kde.org/stable/~a/src/smokeqt-~:*~a.tar.xz" version))))

(defmethod asdf:perform ((op generate-op) (system (eql (asdf:find-system :smokeqt))))
  (with-chdir ((asdf:output-file op system))
    (let ((smoke-dir (asdf:output-file 'install-op (asdf:find-system :smokegen))))
      (run-here "cmake ~s -DCMAKE_BUILD_TYPE=Release ~
                        -DCMAKE_INSTALL_PREFIX=~s ~
                        -DWITH_Qwt5=OFF ~
                        -DSMOKE_INSTALL_PREFIX=~s ~
                        -DSmoke_DIR=~s ~
                        -Wno-dev"
                (first (asdf:input-files op system))
                (asdf:output-file 'install-op system)
                smoke-dir
                (relative-dir smoke-dir "share" "smoke" "cmake"))
      (run-here "env LD_LIBRARY_PATH=\"$LD_LIBRARY_PATH:~a:~a\" make"
                (relative-dir smoke-dir "lib")
                (relative-dir smoke-dir "lib" "smokeqt")))))

(defun smokeqt-on-path-p (path)
  (uiop:file-exists-p
   (make-pathname :name "libsmokeqtcore" :type #+:linux "so" #+:darwin "dylib" #+:windows "dll"
                  :defaults (relative-dir path "lib"))))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokeqt))))
  (loop for dir in '(#+:unix #p"/usr"
                     #+:unix #p"/usr/local"
                     #+:windows #p"C:/Program Files/KDE/smokeqt/")
        when (smokeqt-on-path-p dir)
        return (values (list dir) T)
        finally (return (call-next-method))))
