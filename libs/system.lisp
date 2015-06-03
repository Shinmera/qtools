#|
This file is a part of Qtools
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.libs.generator)

(defclass system ()
  ((version)
   (build-dir)
   (sources-dir)
   (compile-dir)
   (package-dir)))

(defgeneric origin (system &key vc))

(defgeneric download (system &key force))

(defgeneric generate (system &key force))

(defgeneric package (system &key force))

(defgeneric clean (system &key))

(defgeneric build (system &key force))

(defgeneric dependencies (system))

(defgeneric path (system))

(defgeneric ensure (system))
