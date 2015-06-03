#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :libcommonqt
  :class build-system
  :depends-on (:qt-build-prerequisites
               :qt4
               :smokegen
               :smokeqt))

(defun fix-commonqt-pro-file (file &rest basepaths)
  (let ((contents (uiop:read-file-string file)))
    (unless (search "Qtools Fix" contents)
      (with-open-file (stream file :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :error)
        (format stream "~&# Qtools fix~%")
        (dolist (base basepaths)
          (format stream "~&LIBS += -L~s~
                          ~&INCLUDEPATH += ~s"
                  (uiop:native-namestring (relative-dir base "lib/"))
                  (uiop:native-namestring (relative-dir base "include/"))))
        (format stream "~&~a" contents))
      file)))

(defmethod asdf:perform ((op download-op) (system (eql (asdf:find-system :libcommonqt))))
  #+quicklisp
  (unless (ql-dist:installedp (ql-dist:find-system "qt"))
    (ql-dist:install (ql-dist:find-system "qt")))
  #-quicklisp
  (unless (asdf:find-system :qt)
    (cerror "CommonQt is set up." "Please download commonqt and register it with ASDF.")))

(defmethod asdf:output-files ((op download-op) (system (eql (asdf:find-system :libcommonqt))))
  (values (list (asdf:system-source-directory :qt))
          T))

(defmethod asdf:input-files ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (list (make-pathname :name "commonqt" :type "pro" :defaults (asdf:output-file 'download-op system))))

(defmethod asdf:perform ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (let ((project-file (first (asdf:input-files op system))))
    (fix-commonqt-pro-file project-file
                           (asdf:output-file 'install-op (asdf:find-system :smokeqt))
                           (asdf:output-file 'install-op (asdf:find-system :smokegen)))
    (let ((makefile (make-pathname :name "Makefile" :type NIL :defaults project-file)))
      (run-here "`command -v qmake-qt4 || command -v qmake` ~a~s -o ~s"
                #+darwin "-spec macx-g++ " #-darwin ""
                (uiop:native-namestring project-file)
                (uiop:native-namestring makefile))
      (run-here "make -C ~s"
                (uiop:native-namestring (uiop:pathname-directory-pathname makefile))))))

(defmethod asdf:perform ((op install-op) (system (eql (asdf:find-system :libcommonqt))))
  T)

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (values (list (make-pathname :name #-windows "libcommonqt"
                                     #+windows "commonqt"
                               :type #+darwin "dylib"
                                     #+windows "dll"
                                     #-(or darwin windows) "so"
                               :defaults (asdf:output-file 'download-op system)))
          T))

(defmethod asdf:output-files ((op install-op) (system (eql (asdf:find-system :libcommonqt))))
  (asdf:output-files 'generate-op system))

(defmethod shared-library-files ((system (eql (asdf:find-system :libcommonqt))))
  (mapcar #'uiop:resolve-symlinks (asdf:output-files 'generate-op system)))
