#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass build-system-op (asdf:operation)
  ())

(defmethod asdf:perform :before ((op build-system-op) c)
  (status 2 (asdf:action-description op c)))

(defclass download-op (build-system-op asdf:non-propagating-operation)
  ())

(defmethod asdf:action-description ((op download-op) c)
  (format nil "~@<downloading ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op download-op) c)
  NIL)

(defclass generate-op (build-system-op asdf:selfward-operation asdf:sideway-operation)
  ((asdf:selfward-operation :initform 'download-op :allocation :class)
   (asdf:sideway-operation :initform 'install-op :allocation :class)))

(defmethod asdf:action-description ((op generate-op) c)
  (format nil "~@<generating ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op generate-op) c)
  NIL)

(defclass install-op (build-system-op asdf:selfward-operation)
  ((asdf:selfward-operation :initform 'generate-op :allocation :class)))

(defmethod asdf:action-description ((op install-op) c)
  (format nil "~@<installing ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op install-op) c)
  NIL)


(defclass build-system (asdf:system)
  ())

(defgeneric origin (system))

(defgeneric shared-library-files (system))

(defmethod shared-library-files ((system build-system))
  (mapcar #'uiop:resolve-symlinks
          (uiop:directory-files (relative-dir (first (asdf:output-files 'install-op system)) "lib")
                                (make-pathname :type #+darwin "dylib" #+windows "dll" #-(or windows darwin) "so"
                                               :defaults uiop:*wild-file*))))

(defmethod asdf:component-pathname ((system build-system))
  (relative-dir (call-next-method) (asdf:component-name system)))

(defmethod asdf:input-files ((op download-op) (system build-system))
  (list))

(defmethod asdf:perform ((op download-op) (system build-system))
  (let ((version (asdf:component-version system))
        (origin (origin system)))
    (when origin
      (if (eql version :git)
          (clone origin (first (asdf:output-files op system)))
          (with-temp-file (archive (make-pathname :name (format NIL "~a-archive" (asdf:component-name system))
                                                  :type "tar.xz" :defaults (uiop:temporary-directory)))
            (download-file origin archive)
            (extract-tar-archive archive (uiop:pathname-directory-pathname
                                          (first (asdf:output-files op system)))
                                 :strip-folder T))))))

(defmethod asdf:output-files ((op download-op) (system build-system))
  (list (uiop:ensure-directory-pathname "source")))

(defmethod asdf:input-files ((op generate-op) (system build-system))
  (asdf:output-files 'download-op system))

(defmethod asdf:perform ((op generate-op) (system build-system))
  (error "Need to implement ASDF:PERFORM on (~a ~a)" op system))

(defmethod asdf:output-files ((op generate-op) (system build-system))
  (list (uiop:ensure-directory-pathname "generate")))

(defmethod asdf:input-files ((op install-op) (system build-system))
  (asdf:output-files 'generate-op system))

(defmethod asdf:perform ((op install-op) (system build-system))
  (error "Need to implement ASDF:PERFORM on (~a ~a)" op system))

(defmethod asdf:output-files ((op install-op) (system build-system))
  (list (uiop:ensure-directory-pathname "install")))

(defmethod asdf/plan:traverse-action (plan op (c build-system) niip)
  (let ((files (asdf:output-files op c)))
    (unless (and files (loop for file in files always (probe-file file)))
      (call-next-method))))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (system build-system))
  `(,@(call-next-method)
    (install-op ,system)))


(defclass make-build-system (build-system)
  ((make-flags :initarg :make-flags :initform NIL :accessor make-flags)
   (install-flags :initarg :install-flags :initform NIL :accessor install-flags)))

(defmethod asdf:input-files ((op generate-op) (system make-build-system))
  (list* (make-pathname :name "Makefile" :type NIL :defaults (first (call-next-method)))
         (call-next-method)))

(defvar *ld-library-path* (let ((env (uiop:getenv "LD_LIBRARY_PATH")))
                            (if env (list env) ())))
(defmethod asdf:perform ((op generate-op) (system make-build-system))
  (let ((makefile (first (asdf:input-files op system))))
    (run-here "env LD_LIBRARY_PATH=\"~{~a~^:~}\" make -j ~d -C ~a -f ~a~@[.~a~]~@[ ~a~]"
              *ld-library-path* (cpu-count)
              (uiop:pathname-directory-pathname makefile) (pathname-name makefile) (pathname-type makefile)
              (make-flags system))))

(defmethod asdf:perform ((op install-op) (system make-build-system))
  (with-chdir ((first (asdf:input-files op system)))
    (run-here "make install~@[ ~a~]"
              (install-flags system))))


(defclass cmake-build-system (make-build-system)
  ((cmake-flags :initarg :cmake-flags :initform NIL :accessor cmake-flags)))

(defmethod asdf:output-files ((op download-op) (system cmake-build-system))
  (list (make-pathname :name "CMakeLists" :type "txt" :defaults (uiop:ensure-directory-pathname "source"))))

(defmethod asdf:input-files ((op generate-op) (system cmake-build-system))
  (list (make-pathname :name "Makefile" :type NIL :defaults (first (asdf:output-files op system)))
        (make-pathname :name "CMakeLists" :type "txt" :defaults (first (asdf:output-files 'download-op system)))))

(defmethod asdf:perform ((op generate-op) (system cmake-build-system))
  (with-chdir ((first (asdf:output-files op system)))
    (run-here "cmake ~s~@[ ~a~]"
              (uiop:pathname-directory-pathname (second (asdf:input-files op system)))
              (cmake-flags system)))
  (call-next-method))



(asdf:defsystem :qt-build-prerequisites)

(defmethod asdf:perform ((op install-op) (c (eql (asdf:find-system :qt-build-prerequisites))))
  (test-prerequisite "CMake" "cmake")
  (test-prerequisite "Make" "make")
  (test-prerequisite "C Compiler" "cc" "gcc")
  (test-prerequisite "tar" "tar"))

(defun show-plan (op system)
  (loop for (operation . component) in (asdf/plan:plan-actions (asdf:make-plan 'asdf:sequential-plan op (asdf:find-system system)))
        do (format T "~&~15a ~a~%" (type-of operation) (asdf:component-name component))))

;; We /really/ only want the install-op for this.
(defmethod asdf/plan:traverse-action (plan op (c (eql (asdf:find-system :qt-build-prerequisites))) niip)
  (typecase op
    (install-op (call-next-method))
    (T NIL)))
