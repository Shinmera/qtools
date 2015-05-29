#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs)

(defun ensure-commonqt-downloaded ()
  #+quicklisp (ql-dist:ensure-installed (ql-dist:find-system :qt))
  (asdf:system-source-directory
   (asdf:find-system :qt T)))

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

(defun safely-load-commonqt (&key (package-dir (ensure-smokeqt-available)))
  (test-compile-prerequisites)
  (let ((dir (ensure-commonqt-downloaded)))
    (fix-commonqt-pro-file :file (make-pathname :name "commonqt" :type "pro" :defaults dir)
                           :package-dir package-dir)
    #+quicklisp (ql:quickload :qt)
    #-quicklisp (asdf:load-system :qt)
    T))
