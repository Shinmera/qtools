#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qtools-libs
  (:use #:cl)
  (:nicknames #:org.shirakumo.qtools.libs)
  ;; commonqt.lisp
  (:export 
   #:safely-load-commonqt)
  ;; qt4.lisp
  (:export)
  ;; smokegen.lisp
  (:export
   #:*smokegen-origin*
   #:*smokegen-stable-version*
   #:*smokegen-stable-archive*
   #:*smokegen-build-dir*
   #:download-smokegen
   #:compile-smokegen
   #:package-smokegen
   #:clean-smokegen
   #:build-smokegen
   #:smokegen-path
   #:ensure-smokegen-available)
  ;; smokeqt.lisp
  (:export
   #:*smokeqt-origin*
   #:*smokeqt-stable-version*
   #:*smokeqt-stable-archive*
   #:*smokeqt-build-dir*
   #:download-smokeqt
   #:compile-smokeqt
   #:package-smokeqt
   #:clean-smokeqt
   #:build-smokeqt
   #:smokeqt-path
   #:ensure-smokeqt-available)
  ;; toolkit.lisp
  (:export
   #:*bin-dir*
   #:build-dir
   #:sources-dir
   #:compile-dir))
