#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *loaded-foreign-libs* ())
(defvar *loaded-smoke-modules* ())

(defun clean-symbol (symbol)
  (dolist (type '(function compiler-macro setf type variable))
    (when (documentation symbol type)
      (setf (documentation symbol type) NIL))))

(defun prune-symbol (symbol)
  (fmakunbound symbol)
  (makunbound symbol))

(defun prune-package (package)
  (do-symbols (symbol package)
    (when (eql (symbol-package symbol) package)
      (clean-symbol symbol)
      (prune-symbol symbol)))
  (delete-package package))

(defun prune-foreign-libraries ()
  (dolist (lib (cffi:list-foreign-libraries))
    (cffi:close-foreign-library lib)))

(defun prune-image ()
  (do-all-symbols (symbol)
    (clean-symbol symbol))
  (setf cffi:*foreign-library-directories*
        (delete qt-libs:*standalone-libs-dir* cffi:*foreign-library-directories*
                :test #'uiop:pathname-equal))
  (prune-foreign-libraries))

(defun smoke-library-p (lib)
  (flet ((lib-matches-p (libname)
           (eql 0 (search (format NIL #+windows "~a" #-windows "lib~a" libname)
                          (string (cffi:foreign-library-name lib))
                          :test #'char-equal))))
    (or (loop for module in (list* :base *smoke-modules*)
              thereis (lib-matches-p (format NIL "smoke~a" module)))
        (lib-matches-p "commonqt"))))

(defun loaded-foreign-libraries ()
  ;; Remove smoke libraries as we will reload them ourselves later.
  (remove-if #'smoke-library-p (cffi:list-foreign-libraries)))

(defun system-required-libs (system &key (standalone-dir qt-libs:*standalone-libs-dir*))
  (qt-libs:ensure-standalone-libs :standalone-dir standalone-dir)
  (loop for lib in (uiop:directory-files standalone-dir)
        when (flet ((matches (string) (search (string string) (pathname-name lib) :test #'char-equal)))
               (or (matches "smokebase")
                   (matches "commonqt")
                   (loop for module in (qtools:loaded-smoke-modules)
                         thereis (matches module))))
        collect lib))

(defun ensure-system-libs (system target)
  (dolist (lib (system-required-libs system))
    (let ((target (make-pathname :defaults lib :directory (pathname-directory target))))
      (unless (uiop:file-exists-p target)
        (uiop:copy-file lib (ensure-directories-exist target))))))

(defun warmly-boot ()
  (format T "~&[QTOOLS] Performing warm boot.~%")
  (when (uiop:argv0)
    (pushnew (uiop:pathname-directory-pathname (uiop:argv0))
             cffi:*foreign-library-directories*))
  (let (#+sbcl(sb-ext:*muffled-warnings* 'style-warning))
    (qt-libs:load-libcommonqt :force T :ensure-libs NIL)
    (qt::reload)
    (qt:make-qapplication)
    ;; Reload our modules
    (dolist (mod *loaded-smoke-modules*)
      (format T "~&[QTOOLS] (Re-)loading smoke module ~a" mod)
      (qt:ensure-smoke mod))
    ;; Reload Q+
    (process-all-methods)
    ;; Reload other libraries
    (dolist (lib *loaded-foreign-libs*)
      (let ((name (cffi:foreign-library-name lib)))
        (unless (or (cffi:foreign-library-loaded-p lib))
          (format T "~&[QTOOLS] (Re-)loading foreign library ~a" name)
          (cffi:load-foreign-library name))))))

(defun quit ()
  (prune-foreign-libraries)
  (uiop:finish-outputs)
  #+sbcl (sb-ext:exit :timeout 0)
  #-sbcl (uiop:quit :finish-output NIL))

(defun call-entry-prepared (entry-point)
  ;; We don't handle anything here unless we have no other
  ;; choice, as that should otherwise be up to the user.
  ;; Maybe someone will want a debugger in the end
  ;; application. I can't decide that for them, so we leave
  ;; the possibility open.
  (restart-case
      (progn
        (warmly-boot)
        (format T "~&[QTOOLS] Launching application.~%")
        (funcall entry-point)
        (format T "~&[QTOOLS] Epilogue.~%")
        (quit))
    (exit ()
      :report "Exit."
      (quit))))

(defun discover-entry-point (c)
  (let* ((entry (asdf/system:component-entry-point c))
         (class (ignore-errors (uiop:coerce-class entry :super 'qtools:widget :error NIL)))
         (func (ignore-errors (uiop:ensure-function entry))))
    (cond ((not entry)
           (error "~a does not specify an entry point." c))
          (func func)
          (class (lambda () (with-main-window (window (make-instance class)))))
          (T (error "~a's  entry point ~a is not coercable to a widget class or function!" c entry)))))

(defclass qt-program-op (asdf:program-op)
  ())

(defmethod asdf:output-files ((o qt-program-op) (c asdf:system))
  (values (mapcar (lambda (file)
                    (ensure-directories-exist
                     (merge-pathnames (uiop:ensure-directory-pathname "bin") file)))
                  (call-next-method))
          T))

;; Do this before to trick ASDF's subsequent usage of UIOP:ENSURE-FUNCTION on the entry-point slot.
(defmethod asdf:perform :before ((o qt-program-op) (c asdf:system))
  (let ((entry (discover-entry-point c)))
    (setf (asdf/system:component-entry-point c)
          (lambda (&rest args)
            (declare (ignore args))
            (call-entry-prepared entry)))))

(defmethod asdf:perform ((o qt-program-op) (c asdf:system))
  (ensure-system-libs c (uiop:pathname-directory-pathname
                         (first (asdf:output-files o c))))
  (setf *loaded-foreign-libs* (loaded-foreign-libraries))
  (setf *loaded-smoke-modules* (loaded-smoke-modules))
  (prune-image)
  (apply #'uiop:dump-image (first (asdf:output-files o c)) :executable T
         (append #+:sb-core-compression '(:compression T))))

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
