#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

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
  (unless (find-package :drakma)
    (let (#+sbcl (sb-ext:*muffled-warnings* 'style-warning))
      (asdf:load-system :drakma)))
  (with-open-file (output target :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type '(unsigned-byte 8))
    (multiple-value-bind (input status) (funcall (find-symbol (string :http-request) :drakma) url :want-stream T)
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

(defmacro with-chdir ((to) &body body)
  (let ((current (gensym "CURRENT")))
    `(let ((,current (uiop:getcwd)))
       (unwind-protect
            (progn
              (uiop:chdir (ensure-directories-exist ,to))
              ,@body)
         (uiop:chdir ,current)))))

(defmacro with-temp-file ((name pathname) &body body)
  `(let ((,name ,pathname))
     (unwind-protect
          (progn ,@body)
       (uiop:delete-file-if-exists ,name))))

(defun application-available-p (&rest alternatives)
  (zerop (nth-value 2 (uiop:run-program (format NIL "~{command -v ~s~^ || ~}" alternatives) :ignore-error-status T))))

(defun test-prerequisite (name &rest alternatives)
  (with-simple-restart (continue "I know what I'm doing, skip this test.")
    (loop until (if (apply #'application-available-p alternatives)
                    T
                    (with-simple-restart (retry "I installed it now, test again.")
                      (error "~a is required, but could not be found. Please ensure it is installed properly." name))))))
