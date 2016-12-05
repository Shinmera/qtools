#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

;;;;;
;; File processing

(defun write-forms (stream)
  (let ((i 0))
    (map-compile-all
     (lambda (form)
       (incf i)
       (when (= 0 (mod i 1000))
         (format T "~&; On ~dth form..." i))
       (when form
         (dolist (form (if (eql (car form) 'progn)
                           (cdr form)
                           (list form)))
           (print form stream))
         (format stream "~%"))))
    (format T "~&; ~d forms processed." i)))

(defun write-everything-to-file (pathname &key (package *target-package*) (if-exists :supersede))
  (let* ((package (cond ((typep package 'package) package)
                        ((find-package package) (find-package package))
                        (T (make-package package))))
         (modules (loaded-smoke-modules))
         (*target-package* package)
         (*package* (find-package '#:cl-user)))
    (with-open-file (stream pathname :direction :output :if-exists if-exists)
      (format T "~&;;;; Writing to file ~a" pathname)
      (format stream "~&;;;;; Automatically generated file to map Qt methods and enums to CL functions and constants.")
      (format stream "~&;;;;; See QTOOLS:WRITE-EVERYTHING-TO-FILE")
      (format stream "~&;;;;")
      (format stream "~&;;;; Active smoke modules: ~{~a~^ ~}" modules)
      (print `(in-package #:cl-user) stream)
      (print `(eval-when (:compile-toplevel :load-toplevel :execute)
                (unless (find-package "QTOOLS")
                  (error "Qtools needs to be loaded first!"))
                (dolist (module ',modules)
                  (qt:ensure-smoke module))
                (setf qtools:*target-package*
                      (or (find-package ,(package-name package))
                          (make-package ,(package-name package) :use ())))) stream)
      (write-forms stream)
      pathname)))

(defun q+-compile-and-load (&key modules (file (merge-pathnames "q+.lisp" (uiop:temporary-directory))))
  (when modules
    (qt::reload)
    (apply #'load-all-smoke-modules modules))
  (load (compile-file (write-everything-to-file file) :print NIL) :print NIL))


;;;;;
;; ASDF components
(defun get-platform-dependencies (c)
  (let ((dependencies (getf (dependencies c)
                            #+windows :win
                            #+linux :lin
                            #+darwin :mac
                            :none)))
    (unless (eql dependencies :none)
      (return-from get-platform-dependencies dependencies)))
  (let ((dependencies (getf (dependencies c)
                            T
                            :none)))
    (unless (eql dependencies :none)
      (return-from get-platform-dependencies dependencies)))
  (error "~a cannot be loaded on this platform." c))

(defun load-for-wrapper (c)
  (dolist (dep (get-platform-dependencies c))
    (asdf:load-system dep))
  (dolist (lib (library-files c))
    (qt-libs:ensure-lib-loaded lib))
  (etypecase (smoke-module c)
    ((or symbol string) (load-all-smoke-modules (smoke-module c)))
    (list (apply #'load-all-smoke-modules (smoke-module c))))
  T)

(defclass smoke-module-system (asdf:system)
  ((smoke-module :accessor smoke-module :initarg :module :initform NIL)
   (library-files :accessor library-files :initarg :library-files :initform NIL)
   (dependencies :accessor dependencies :initarg :dependencies :initform '(T ()))))

(defmethod asdf:perform ((op asdf:compile-op) (c smoke-module-system))
  (load-for-wrapper c))

(defmethod asdf:perform ((op asdf:load-op) (c smoke-module-system))
  (load-for-wrapper c))

(defun compile-smoke-module-system-definition (module &key dependencies library-files)
  `(asdf:defsystem ,(make-symbol (string-upcase module))
     :defsystem-depends-on (:qtools)
     :class "qtools::smoke-module-system"
     :version "1.0.0"
     :license "Artistic"
     :author "Nicolas Hafner <shinmera@tymoon.eu>"
     :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
     :description ,(format NIL "ASDF System wrapper around the ~a smoke module. Ensures that it is present during compilation and loading of a system."
                           module)
     :module ,(string-upcase module)
     :library-files ,library-files
     :dependencies ,dependencies))

(defun write-smoke-module-system-file (module &key (dependencies '(T ())) library-files
                                                   (path (asdf:system-relative-pathname :qtools (format NIL "smoke/~(~a~).asd" module))))
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (let ((*package* (find-package :cl-user))
          (*print-case* :downcase)
          (*print-pretty* T))
      (print '(in-package #:cl-user) stream)
      (print (compile-smoke-module-system-definition module :dependencies dependencies
                                                            :library-files library-files)
             stream)))
  path)

;; Manually gathered from ldd/otool/depwalker information about the libraries
(defun generate-smoke-module-systems ()
  (macrolet ((g (&body defs)
               `(list
                  ,@(loop for def in defs
                          collect (destructuring-bind (module &key (dependencies '(T ())) library-files) def
                                    
                                    `(write-smoke-module-system-file
                                      ',module :dependencies ',dependencies
                                               :library-files ',library-files))))))
    (g (phonon
        :dependencies (:win (:qtcore :qtgui)
                       :lin (:qtcore :qtgui :qtdbus :qtxml)
                       :mac (:qtcore :qtgui :qtdbus :qtxml))
        :library-files ("phonon"))
       (qimageblitz
        :dependencies (T (:qtcore :qtgui))
        :library-files ("qimageblitz"))
       (qsci
        :dependencies (T (:qtcore :qtgui))
        :library-files ("qscintilla2"))
       (qt3support
        :dependencies (T (:qtcore :qtgui :qtxml :qtnetwork :qtsql))
        :library-files ("Qt3Support"))
       (qtcore
        :library-files ("QtCore"))
       (qtdbus
        :dependencies (:lin (:qtcore :qtxml)
                       :mac (:qtcore :qtxml))
        :library-files ("QtDBus"))
       (qtdeclarative
        :dependencies (T (:qtcore :qtgui :qtnetwork :qtscript :qtsql :qtxmlpatterns))
        :library-files ("QtDeclarative"))
       (qtgui
        :dependencies (T (:qtcore))
        :library-files ("QtGui"))
       (qthelp
        :dependencies (T (:qtcore :qtgui :qtnetwork :qtsql))
        :library-files ("QtHelp" "QtCLucene"))
       (qtnetwork
        :dependencies (T (:qtcore))
        :library-files ("QtNetwork"))
       (qtopengl
        :dependencies (T (:qtcore :qtgui))
        :library-files ("QtOpenGL"))
       (qtscript
        :dependencies (T (:qtcore))
        :library-files ("QtScript"))
       (qtsql
        :dependencies (T (:qtcore :qtgui))
        :library-files ("QtSql"))
       (qtsvg
        :dependencies (T (:qtcore :qtgui))
        :library-files ("QtSvg"))
       (qttest
        :dependencies (T (:qtcore :qtgui))
        :library-files ("QtTest"))
       (qtuitools
        :dependencies (T (:qtcore :qtgui)))
       (qtwebkit
        :dependencies (T (:qtcore :qtgui :qtnetwork))
        :library-files ("QtWebKit"))
       (qwt
        :dependencies (:win (:qtcore :qtgui :qtsvg)
                       :lin (:qtcore :qtgui)
                       :mac (:qtcore :qtgui))
        :library-files ("qwt"))
       (qtxmlpatterns
        :dependencies (T (:qtcore :qtnetwork))
        :library-files ("QtXmlPatterns"))
       (qtxml
        :dependencies (T (:qtcore))
        :library-files ("QtXml")))))
