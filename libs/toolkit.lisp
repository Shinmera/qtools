#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defvar *bin-dir* (ensure-directories-exist
                   (asdf:system-relative-pathname :qtools-lib-generator "built" :type :directory)))

(defun externalize (thing)
  (typecase thing
    (list thing)
    (string thing)
    (pathname (uiop:native-namestring thing))
    (T (princ-to-string thing))))

(defun status (n string &rest format-args)
  (format T "~&~a ~a~%"
          (case n (0 ">") (1 " ->") (2 " ==>") (T "  >>>"))
          (apply #'format NIL string format-args)))

(defun run-here (string &rest format-args)
  (let ((program (apply #'format NIL string (mapcar #'externalize format-args))))
    (status 1 "Running ~a" program)
    (uiop:run-program program :output T :error-output T)))

(defun clone (origin target)
  (test-prerequisite "GIT" "git")
  (status 2 "Cloning ~a" origin)
  (run-here "git clone ~s ~s" origin target))

(defun download-file (url target)
  (status 2 "Downloading ~a" url)
  (with-open-file (output target :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
    (multiple-value-bind (input status) (drakma:http-request url :want-stream T)
      (unwind-protect
           (progn
             (unless (= status 200)
               (error "Bad status code: ~s" status))
             (loop for byte = (read-byte input NIL NIL)
                   while byte
                   do (write-byte byte output)))
        (close input)))))

(defun extract-tar-archive (from to &key (strip-folder))
  (test-prerequisite "tar" "tar")
  (status 2 "Extracting ~a" (uiop:native-namestring from))
  (run-here "tar ~@[--strip-components=1 ~*~] -xpf ~s -C ~s" strip-folder from to))

(defun relative-dir (relative &rest subdirs)
  (loop for sub in subdirs
        for dir = (merge-pathnames (uiop:ensure-directory-pathname sub)
                                   (uiop:ensure-directory-pathname relative))
        then (merge-pathnames (uiop:ensure-directory-pathname sub) dir)
        finally (return dir)))

(defun build-dir (name)
  (ensure-directories-exist
   (relative-dir (uiop:temporary-directory) name)))

(defun sources-dir (build-dir)
  (ensure-directories-exist
   (relative-dir build-dir "source")))

(defun compile-dir (build-dir)
  (ensure-directories-exist
   (relative-dir build-dir "compile")))

(defmacro with-chdir ((to) &body body)
  (let ((current (gensym "CURRENT")))
    `(let ((,current (uiop:getcwd)))
       (unwind-protect
            (progn
              (uiop:chdir ,to)
              ,@body)
         (uiop:chdir ,current)))))

(defun application-available-p (&rest alternatives)
  (zerop (nth-value 2 (uiop:run-program (format NIL "~{command -v ~s~^ || ~}" alternatives) :ignore-error-status T))))

(defun test-prerequisite (name &rest alternatives)
  (with-simple-restart (continue "I know what I'm doing, skip this test.")
    (loop until (if (apply #'application-available-p alternatives)
                    T
                    (with-simple-restart (retry "I installed it now, test again.")
                      (error "~a is required, but could not be found. Please ensure it is installed properly." name))))))

(defun test-compile-prerequisites ()
  (test-prerequisite "CMake" "cmake")
  (test-prerequisite "Make" "make")
  (test-prerequisite "C Compiler" "cc" "gcc")
  (test-prerequisite "Qt4.8" "qmake-qt4" "qmake")
  (test-prerequisite "tar" "tar")
  T)

(defun determine-so-type (pathname)
  (cond ((search ".so." (pathname-name pathname))
         "so")
        (T (pathname-type pathname))))

(defun determine-so-name (pathname)
  (Cond ((search ".so." (pathname-name pathname))
         (subseq (pathname-name pathname) 0 (search ".so." (pathname-name pathname))))
        (T (pathname-name pathname))))

(defun so-file (name &optional defaults)
  (make-pathname :type #+darwin "dylib" #+windows "dll" #-(or darwin windows) "so"
                 :name name :defaults defaults))
