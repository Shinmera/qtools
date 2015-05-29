#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qtools-lib-generator
  (:use #:cl)
  (:nicknames #:org.shirakumo.qtools.libs.generator)
  ;; commonqt.lisp
  (:export 
   #:download-libcommonqt
   #:compile-libcommonqt
   #:package-libcommonqt
   #:clean-libcommonqt
   #:build-libcommonqt
   #:libcommonqt-path
   #:ensure-libcommonqt)
  ;; deploy.lisp
  (:export
   #:*standalone-libs-dir*
   #:ensure-standalone-libs
   #:libsmokebase
   #:libcommonqt
   #:load-libcommonqt)
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
   #:ensure-smokegen)
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
   #:ensure-smokeqt)
  ;; toolkit.lisp
  (:export
   #:*bin-dir*
   #:build-dir
   #:sources-dir
   #:compile-dir))
