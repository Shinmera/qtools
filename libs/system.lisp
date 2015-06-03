#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defgeneric origin (system))

(defclass download-op (asdf:non-propagating-operation)
  ())

(defmethod asdf:action-description ((op download-op) c)
  (format nil "~@<downloading ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op download-op) c)
  NIL)

(defclass generate-op (asdf:selfward-operation)
  ((asdf:selfward-operation :initform 'download-op :allocation :class)))

(defmethod asdf:action-description ((op generate-op) c)
  (format nil "~@<generating ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op generate-op) c)
  NIL)

(defclass install-op (asdf:selfward-operation)
  ((asdf:selfward-operation :initform 'generate-op :allocation :class)))

(defmethod asdf:action-description ((op install-op) c)
  (format nil "~@<installing ~3i~_~A~@:>" c))

(defmethod asdf:perform ((op install-op) c)
  NIL)



(defclass build-system (asdf:system)
  ())

(defmethod asdf:component-pathname ((system build-system))
  (relative-dir (call-next-method) (asdf:component-name system)))

(defmethod asdf:input-files ((op download-op) (system build-system))
  (list))

(defmethod asdf:perform ((op download-op) (system build-system))
  (let ((version (asdf:component-version system))
        (origin (origin system)))
    (when origin
      (if (eql version :git)
          (clone origin (asdf:output-file op system))
          (with-temp-file (archive (make-pathname :name (format NIL "~a-archive" (asdf:component-name system))
                                                  :type "tar.xz" :defaults (uiop:temporary-directory)))
            (download-file origin archive)
            (extract-tar-archive archive (asdf:output-file op system) :strip-folder T))))))

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
  (with-chdir ((first (asdf:input-files op system)))
    (run-here "make install")))

(defmethod asdf:output-files ((op install-op) (system build-system))
  (list (uiop:ensure-directory-pathname "install")))

(defmethod asdf:component-depends-on ((op asdf:compile-op) (system build-system))
  `(,@(call-next-method)
    (install-op ,system)))

(asdf:defsystem :build-prerequisites)

(defmethod asdf:perform ((op install-op) (c (eql (asdf:find-system :build-prerequisites))))
  (test-prerequisite "CMake" "cmake")
  (test-prerequisite "Make" "make")
  (test-prerequisite "C Compiler" "cc" "gcc")
  (test-prerequisite "tar" "tar"))

(defmethod asdf:needed-in-image-p ((op download-op) (c (eql (asdf:find-system :build-prerequisites))))
  NIL)

(defmethod asdf:needed-in-image-p ((op install-op) (c (eql (asdf:find-system :build-prerequisites))))
  T)
