#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *foreign-libraries-to-reload* ())
(defvar *smoke-modules-to-reload* ())
(defvar *build-hooks* (list 'prune-image))
(defvar *boot-hooks* (list 'boot-foreign-libraries))
(defvar *quit-hooks* (list))
(defvar *folder-listing-cache* (make-hash-table :test 'equal))
(defvar *user-libs* ())
(defvar *deployment-location* NIL)

(defun status (level format-string &rest format-args)
  (format T "~& ~a ~?~%" (case level
                           (0 "==>")
                           (1 "  ->")
                           (T "    >"))
          format-string format-args))

(defun user-libs (name)
  (cdr (assoc name *user-libs*)))

(defun (setf user-libs) (libs name)
  (let ((cons (assoc name *user-libs*)))
    (cond (cons
           (setf (cdr cons) libs))
          (*user-libs*
           (setf (cdr (last *user-libs*)) (cons name libs)))
          (T
           (setf *user-libs* (list (cons name libs)))))))

(defun remove-user-libs (name)
  (setf *user-libs* (remove name *user-libs* :key #'first)))

(defmacro define-user-libs ((name &rest search-paths) &body libs)
  `(setf (user-libs ',name)
         (list (list ,@search-paths)
               (list ,@(loop for lib in libs
                             for (name . paths) = (ensure-list lib)
                             collect `(list ',name ,@paths))))))

(defun user-libs-paths ()
  (loop for entry in *user-libs*
        for (search-paths libs) = (cdr entry)
        append (loop for (name . paths) in libs
                     for known-paths = (remove-if-not #'uiop:file-exists-p paths)
                     for auto-path = (discover-foreign-library-location name search-paths)
                     append (cond (known-paths known-paths)
                                  (auto-path (list auto-path))
                                  (T (warn "No suitable path for ~a found, not copying." name))))))

(defun discover-foreign-library-location (lib &optional search-paths)
  ;; FIXME: Maybe do something with /etc/ld.so.cache ?
  (let ((libpath (etypecase lib
                   (symbol (cffi:foreign-library-pathname (cffi::get-foreign-library lib)))
                   (cffi:foreign-library (cffi:foreign-library-pathname lib))
                   (pathname lib)
                   (string (uiop:parse-native-namestring lib))))
        (search-paths (append search-paths
                              cffi:*foreign-library-directories*
                              #+unix (qt-lib-generator:get-path "LD_LIBRARY_PATH")
                              #+darwin (qt-lib-generator:get-path "DYLD_LIBRARY_PATH")
                              #+windows (qt-lib-generator:get-path "PATH")
                              #+unix '(#p"/lib/"
                                       #p"/usr/lib/"
                                       #p"/usr/local/lib/")
                              #+windows '(#p"C:/Windows/"
                                          #p"C:/Windows/System32/"
                                          #p"C:/Windows/SysWOW64/"))))
    (if (eql :absolute (first (pathname-directory libpath)))
        libpath
        (dolist (path search-paths)
          (dolist (file (or (gethash path *folder-listing-cache*)
                            (setf (gethash path *folder-listing-cache*)
                                  (uiop:directory-files path))))
            (when (and (not (uiop:directory-pathname-p file))
                       (equal (pathname-name libpath) (pathname-name file))
                       (equal (pathname-type libpath) (pathname-name file)))
              (return-from discover-foreign-library-location file)))))))

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

(defun prune-local-paths ()
  (setf cffi:*foreign-library-directories*
        (delete qt-libs:*standalone-libs-dir* cffi:*foreign-library-directories*
                :test #'uiop:pathname-equal))
  (setf qt-libs:*standalone-libs-dir* ".")
  (setf *folder-listing-cache* NIL)
  (setf *user-libs* NIL))

(defun prune-foreign-libraries ()
  (dolist (lib (cffi:list-foreign-libraries))
    (let ((name (cffi:foreign-library-name lib)))
      (when (cffi:foreign-library-loaded-p lib)
        (status 1 "Closing foreign library ~a." name)
        (cffi:close-foreign-library name)))))

(defun prune-image ()
  (status 1 "Pruning the image.")
  (do-all-symbols (symbol)
    (clean-symbol symbol))
  (setf qt:*qapplication* NIL)
  ;; Force CommonQt to forget all prior information it might have had.
  (qt::reload)
  #+:verbose (v:remove-global-controller)
  (prune-local-paths)
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

(defun system-required-libs (&key (standalone-dir qt-libs:*standalone-libs-dir*))
  (qt-libs:ensure-standalone-libs :standalone-dir standalone-dir)
  (loop for lib in (uiop:directory-files standalone-dir)
        when (flet ((matches (string) (search (string string) (file-namestring lib) :test #'char-equal)))
               (or (matches "smokebase")
                   (matches "commonqt")
                   ;; Extra requirements for OS X
                   #+osx-ports (and (matches "z") (find :qtcore (qtools:loaded-smoke-modules)))
                   #+osx-ports (and (matches "png") (find :qtgui (qtools:loaded-smoke-modules)))
                   #+osx-ports (and (matches "crypto") (find :qtnetwork (qtools:loaded-smoke-modules)))
                   #+osx-ports (and (matches "ssl") (find :qtnetwork (qtools:loaded-smoke-modules)))
                   #+osx-ports (and (matches "dbus") (find :qtdbus (qtools:loaded-smoke-modules)))
                   (loop for module in (qtools:loaded-smoke-modules)
                         thereis (matches module))))
        collect lib))

(defun deploy-foreign-libraries (libraries target)
  (ensure-directories-exist target)
  (dolist (lib libraries)
    (let ((target (make-pathname :defaults lib :directory (pathname-directory target))))
      (unless (uiop:file-exists-p target)
        (status 1 "Copying library ~a" lib)
        (uiop:copy-file lib target)))))

(defun boot-foreign-libraries ()
  (flet ((maybe-load (lib)
           (let* ((lib (cffi::get-foreign-library lib))
                  (name (cffi:foreign-library-name lib)))
             (unless (cffi:foreign-library-loaded-p lib)
               (status 1 "Loading foreign library ~a." name)
               (cffi:load-foreign-library name)))))
    (dolist (lib *foreign-libraries-to-reload*)
      (maybe-load lib))))

(defun warmly-boot ()
  (status 0 "Performing warm boot.")
  #+:verbose (v:restart-global-controller)
  (when (uiop:argv0)
    (setf qt-libs:*standalone-libs-dir*
          (uiop:pathname-directory-pathname (uiop:argv0))))
  (let (#+sbcl(sb-ext:*muffled-warnings* 'style-warning))
    ;; Reload libcommonqt core safely
    (qt-libs:load-libcommonqt :force T)
    ;; Reload our modules, but without ASDF trying to probe the damn files.
    (let ((asdf:*system-definition-search-functions* ()))
      (dolist (mod *smoke-modules-to-reload*)
        (status 1 "Loading smoke module ~a." mod)
        (asdf:load-system mod :force T)))
    ;; Reload Q+
    (process-all-methods)
    (status 0 "Running boot hooks.")
    (mapc #'funcall *boot-hooks*)))

(defun quit ()
  (status 0 "Running quit hooks.")
  (mapc #'funcall *quit-hooks*)
  (qt:optimized-delete qt:*qapplication*)
  (uiop:finish-outputs)
  #+sbcl (sb-ext:exit :timeout 1)
  #-sbcl (uiop:quit 0 NIL))

(defun call-entry-prepared (entry-point)
  ;; We don't handle anything here unless we have no other
  ;; choice, as that should otherwise be up to the user.
  ;; Maybe someone will want a debugger in the end
  ;; application. I can't decide that for them, so we leave
  ;; the possibility open.
  (restart-case
      (progn
        (warmly-boot)
        (status 0 "Launching application.")
        (funcall entry-point)
        (status 0 "Epilogue.")
        (invoke-restart 'exit))
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

(defun compute-libraries-to-reload ()
  (let* ((user-libs (loop for entry in *user-libs*
                          append (mapcar #'first (third entry))))
         (other-libs (remove-if (lambda (lib) (find lib user-libs))
                                (mapcar #'cffi:foreign-library-name
                                        (loaded-foreign-libraries)))))
    (append user-libs other-libs)))

(defmethod asdf:perform ((o qt-program-op) (c asdf:system))
  (let ((*deployment-location* (uiop:pathname-directory-pathname (first (asdf:output-files o c)))))
    (status 0 "Gathering system information.")
    (setf *smoke-modules-to-reload* (loaded-smoke-modules))
    (setf *foreign-libraries-to-reload* (compute-libraries-to-reload))
    (status 1 "Will load the following smoke modules on boot: ~s" *smoke-modules-to-reload*)
    (status 1 "Will load the following foreign libs on boot:  ~s" *foreign-libraries-to-reload*)
    (status 0 "Copying necessary libraries.")
    (deploy-foreign-libraries (system-required-libs) *deployment-location*)
    (deploy-foreign-libraries (user-libs-paths) *deployment-location*)
    (status 0 "Running build hooks.")
    (mapc #'funcall *build-hooks*)
    (status 0 "Dumping image.")
    (let ((file (first (asdf:output-files o c))))
      #+(and windows ccl)
      (ccl:save-application file
                            :prepend-kernel T :purify T
                            :application-type :gui
                            :toplevel-function #'uiop:restore-image)
      #-(and windows ccl)
      (uiop:dump-image file
                       :executable T
                       #+sb-core-compression :compression #+sb-core-compression T
                       #+(and sbcl os-windows) :application-type #+(and sbcl os-windows) :gui))))

(defun build-qt-system (system &rest keys &key force force-not verbose version &allow-other-keys)
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
