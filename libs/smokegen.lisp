#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :smokegen
  :class build-system
  :version "4.14.3"
  :depends-on (:qt-build-prerequisites))

(defmethod origin ((system (eql (asdf:find-system :smokegen))))
  (let ((version (asdf:component-version system)))
    (if (eql version :git)
        "git://anongit.kde.org/smokegen"
        (format NIL "http://download.kde.org/stable/~a/src/smokegen-~:*~a.tar.xz" version))))

(defmethod asdf:perform ((op generate-op) (system (eql (asdf:find-system :smokegen))))
  (with-chdir ((asdf:output-file op system))
    (run-here "cmake ~s -DCMAKE_BUILD_TYPE=Release ~
                        -DCMAKE_INSTALL_PREFIX=~s"
              (first (asdf:input-files op system))
              (asdf:output-file 'install-op system))
    (run-here "make")))

(defun smokegen-on-path-p (path)
  (uiop:file-exists-p
   (make-pathname :name "smokegen" :defaults (relative-dir path "bin"))))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :smokegen))))
  (loop for dir in '(#+:unix #p"/usr"
                     #+:unix #p"/usr/local"
                     #+:windows #p"C:/Program Files/KDE/smokegen/")
        when (smokegen-on-path-p dir)
        return (values (list dir) T)
        finally (return (call-next-method))))
