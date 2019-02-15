#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *smoke-modules-to-reload* ())
(defvar *build-hooks* ())
(defvar *boot-hooks* ())
(defvar *quit-hooks* ())
(defvar *deployment-location* NIL)

(deploy:define-hook (:deploy qtools) (directory)
  (when (find :deploy-qt-plugins *features*)
    (deploy:status 1 "Copying Qt plugins directory.")
    (deploy:copy-directory-tree
     (merge-pathnames "plugins/" qt-libs:*standalone-libs-dir*)
     directory)))

(deploy:define-hook (:build qtools) ()
  (setf *smoke-modules-to-reload* (loaded-smoke-modules))
  (deploy:status 1 "Will load the following smoke modules on boot:
      ~s" *smoke-modules-to-reload*)
  (deploy:status 1 "Calling Qtools build hooks.")
  (mapc #'funcall *build-hooks*)
  (deploy:status 1 "Pruning the image.")
  (setf qt:*qapplication* NIL)
  ;; Force CommonQt to forget all prior information it might have had.
  (qt::reload)
  (when (find-package :verbose)
    (funcall (find-symbol (string :remove-global-controller) :verbose)))
  ;; Force ASDF to clear the qt wrapper systems and register them as preloaded.
  (dolist (definition qt-libs::*foreign-library-systems-data*)
    (let ((system (first definition)))
      (asdf:register-preloaded-system system)
      (asdf:clear-system system)))
  (setf cffi:*foreign-library-directories* ())
  (setf qt-libs:*standalone-libs-dir* "."))

(deploy:define-hook (:boot qtools (1+ most-positive-fixnum)) ()
  (when (find-package :verbose)
    (funcall (find-symbol (string :restart-global-controller) :verbose)))
  (setf qt-libs:*standalone-libs-dir* (deploy:data-directory))
  (qt-libs:setup-paths)
  (let (#+sbcl(sb-ext:*muffled-warnings* 'style-warning))
    ;; Reload libcommonqt core safely
    (qt-libs::%ensure-lib-loaded "smokebase")
    (qt-libs::%ensure-lib-loaded "commonqt")
    (qt::reload)
    ;; Reload our modules
    (dolist (mod *smoke-modules-to-reload*)
      (deploy:status 1 "Loading smoke module ~a." mod)
      (qt-libs:manually-load-foreign-library-system mod))
    ;; Reload Q+
    (process-all-methods)
    (deploy:status 0 "Running Qtools boot hooks.")
    (mapc #'funcall *boot-hooks*)))

(deploy:define-hook (:quit qtools) ()
  (loop for func in *quit-hooks*
        do (handler-case (funcall func)
             (error (err)
               (deploy:status 1 "Error during quit: ~a" err)))))

(defun qtools-library-p (lib)
  (flet ((lib-matches-p (libname)
           (string-equal (string libname) (string (deploy:library-name lib)))))
    (or (loop for module in (list* :base *smoke-modules*)
              thereis (or (lib-matches-p module)
                          (lib-matches-p (format NIL "smoke~a" module))))
        (lib-matches-p "commonqt"))))

;; Override
(defmethod shared-initialize :after ((library deploy:library) slots &key)
  (setf (deploy:library-path library) (deploy:find-source-file library))
  (when (qtools-library-p library)
    (setf (deploy:library-dont-open-p library) T)))

(defclass qt-program-op (deploy:deploy-op)
  ())

(defmethod deploy:discover-entry-point ((o qt-program-op) (c asdf:system))
  (let* ((entry (asdf/system:component-entry-point c))
         (class (ignore-errors (uiop:coerce-class entry :super 'qtools:widget :error NIL)))
         (func (ignore-errors (uiop:ensure-function entry))))
    (cond ((not entry)
           (error "~a does not specify an entry point." c))
          (func func)
          (class (lambda () (with-main-window (window (make-instance class)))))
          (T (error "~a's  entry point ~a is not coercable to a widget class or function!" c entry)))))

(defmethod asdf:perform :before ((o qt-program-op) (c asdf:system))
  (setf *deployment-location* (second (asdf:output-files o c))))

;; hook ASDF
(flet ((export! (symbol package)
         (import symbol package)
         (export symbol package)))
  (export! 'qt-program-op :asdf/bundle)
  (export! 'qt-program-op :asdf))
