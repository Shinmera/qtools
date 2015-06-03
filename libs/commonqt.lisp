#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(asdf:defsystem :libcommonqt
  :class build-system
  :depends-on (:build-prerequisites
               :qt4
               :smokegen
               :smokeqt))

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

(defmethod asdf:perform ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (fix-commonqt-pro-file :file (make-pathname :name "commonqt" :type "pro" :defaults (asdf:output-file 'download-op system))
                         :package-dir (asdf:output-file 'install-op (asdf:find-system :smokeqt)))
  (asdf:compile-system :qt))

(defmethod asdf:perform ((op install-op) (system (eql (asdf:find-system :libcommonqt))))
  T)

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

(defmethod asdf:output-files ((op generate-op) (system (eql (asdf:find-system :libcommonqt))))
  (values (list (make-pathname :name #-windows "libcommonqt"
                                     #+windows "commonqt"
                               :type #+darwin "dylib"
                                     #+windows "dll"
                                     #-(or darwin windows) "so"
                               :defaults (asdf:output-file 'download-op system)))
          T))
