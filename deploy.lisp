#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defun clean-symbol (symbol)
  (dolist (type '(function compiler-macro setf type variable))
    (when (documentation symbol type)
      (setf (documentation symbol type) NIL))))

(defun prune-symbol (symbol)
  (fmakunbound symbol)
  (makunbound symbol))

(defun prune-package (package)
  (do-symbols (symbol package)
    (clean-symbol symbol)
    (prune-symbol symbol))
  (delete-package package))

(defun prune-image ()
  (do-all-symbols (symbol)
    (clean-symbol symbol))
  (tg:gc :full T))


(defclass qt-program-op (asdf:program-op)
  ())

(defun qt-entry-point (c)
  (let* ((entry (asdf/system:component-entry-point c))
         (class (ignore-errors (uiop:coerce-class entry :super 'qtools:widget :error NIL)))
         (func (ignore-errors (uiop:ensure-function entry))))
    (cond ((not entry)
           (error "~a does not specify an entry point." c))
          (class (lambda () (with-main-window (window (make-instance class)))))
          (func func)
          (T (error "~a's  entry point ~a is not coercable to a widget class or function!" c entry)))))

(defun system-required-libs (system &key (standalone-dir qt-libs:*standalone-libs-dir*))
  (qt-libs:ensure-standalone-libs :standalone-dir standalone-dir)
  (loop for lib in (uiop:directory-files standalone-dir)
        when (flet ((matches (string) (search string (pathname-name lib) :test #'char-equal)))
               (or (matches "smokebase")
                   (matches "commonqt")
                   (loop for dep in (asdf:system-depends-on system)
                         thereis (and (typep (asdf:find-system dep) 'smoke-module-system)
                                      (matches dep)))))
        collect lib))

(defun ensure-system-libs (system target)
  (dolist (lib (system-required-libs system))
    (let ((target (make-pathname :defaults lib :directory (pathname-directory target))))
      (unless (uiop:file-exists-p target)
        (uiop:copy-file lib (ensure-directories-exist target))))))

(defmethod asdf:output-files ((o qt-program-op) (c asdf:system))
  (values (mapcar (lambda (file)
                    (ensure-directories-exist
                     (merge-pathnames (uiop:ensure-directory-pathname "bin") file)))
                  (call-next-method))
          T))

;; Do this before to trick ASDF's subsequent usage of UIOP:ENSURE-FUNCTION on the entry-point slot.
(defmethod asdf:perform :before ((o qt-program-op) (c asdf:system))
  (let ((entry (qt-entry-point c)))
    (setf (asdf/system:component-entry-point c)
          (lambda (&rest args)
            (declare (ignore args))
            (format T "~&Loading shared libraries...~%")
            (qt-libs:load-libcommonqt :force T)
            (qt::reload)
            (format T "~&Launching application.~%")
            (funcall entry)))))

(defmethod asdf:perform ((o qt-program-op) (c asdf:system))
  (ensure-system-libs c (uiop:pathname-directory-pathname (asdf:output-file o c)))
  (prune-image)
  (call-next-method))

(defun build-qt-system (system &rest keys &key force force-not verbose version &allow-other-keys)
  "Shorthand for `(operate 'asdf:qt-program-op system)`. See OPERATE for details."
  (declare (ignore force force-not verbose version))
  (apply 'asdf:operate 'qt-program-op system keys)
  T)

;; hook ASDF
(flet ((export! (symbol package)
         (import symbol package)
         (export symbol package)))
  (export! 'qt-program-op :asdf/bundle)
  (export! 'build-qt-system :asdf/operate)
  (dolist (symbol '(qt-program-op build-qt-system))
    (export! symbol :asdf)))
