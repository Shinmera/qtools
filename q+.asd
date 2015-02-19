(in-package #:cl-user)
(defpackage #:org.shirakumo.qtools.q+.asd
  (:use #:cl #:asdf))
(in-package #:org.shirakumo.qtools.q+.asd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :qtools))

;; Wrapper systems
(defclass smoke-wrapper (cl-source-file)
  ((smoke-module :accessor smoke-module :initarg :module :initform (error "MODULE required."))))

(defmethod perform ((op prepare-op) (c smoke-wrapper))
  ;; There's, apparently, no way to catch whether :force was passed
  ;; to the current operation, so we'll have to resort to only
  ;; regenerating if the file does not yet exist.
  (unless (uiop:file-exists-p (component-pathname c))
    (qt::reload)
    (etypecase (smoke-module c)
      (string (qt:ensure-smoke (smoke-module c)))
      (list (qtools::load-all-smoke-modules (smoke-module c))))
    (qtools::process-all-methods)
    (qtools::write-everything-to-file (component-pathname c))))

(defmacro define-q+-system (name &optional (module name) dependencies)
  (let ((name (string-downcase name)))
    `(defsystem ,(make-symbol (format NIL "Q+-~:@(~a~)" name))
       :source-file ,(merge-pathnames "q+.asd" (system-source-file :qtools))
       :components ((:module "q+"
                     :components ((:smoke-wrapper ,name :module ,module))))
       :depends-on (:qtools ,@dependencies))))

;; Smoke module systems
(defclass smoke-module-system (system)
  ((smoke-module :accessor smoke-module :initarg :module :initform NIL)))

(defmethod perform ((op load-op) (c smoke-module-system))
  (etypecase (smoke-module c)
    (string (qt:ensure-smoke (smoke-module c)))
    (list (qtools::load-all-smoke-modules (smoke-module c))))
  T)

(defmacro define-smoke-module-system (name module)
  `(defsystem ,name
     :class :smoke-module-system
     :module ,module))

(macrolet ((define-all-smoke-module-systems ()
             `(progn ,@(loop for module in qtools::*smoke-modules*
                             collect `(define-smoke-module-system ,module ,module)))))
  (define-all-smoke-module-systems))

;; General Q+ system
(defclass dynamic-smoke-wrapper (cl-source-file)
  ())

(defmethod perform ((op prepare-op) (c dynamic-smoke-wrapper))
  (let ((module-buffer (merge-pathnames "q+modules.lisp-expr" (component-pathname c))))
    (unless (and (uiop:file-exists-p (component-pathname c))
                 (equal (qtools::loaded-smoke-modules)
                        (with-open-file (stream module-buffer :if-does-not-exist NIL)
                          (when stream (read stream)))))
      (qtools::process-all-methods)
      (qtools::write-everything-to-file (component-pathname c))
      (with-open-file (stream module-buffer :direction :output :if-exists :supersede)
        (print (qtools::loaded-smoke-modules) stream)))))

(macrolet ((def ()
             `(defsystem #:q+
                :source-file ,(merge-pathnames "q+.asd" (system-source-file :qtools))
                :components ((:module "q+"
                              :components ((:dynamic-smoke-wrapper "q+"))))
                :depends-on (:qtools))))
  (def))
