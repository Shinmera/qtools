#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.game)
(named-readtables:in-readtable :qtools)

;;;;;
;; Base
(defclass chunk (positioned-rectangle)
  ())

(defclass real-chunk (chunk)
  ())

(defclass virtual-chunk (chunk)
  ())

(defmethod print-object ((chunk chunk) stream)
  (if *print-readably*
      (print
       `(make-instance ',(class-name (class-of chunk))
                       ,@(make-args chunk))
       stream)
      (print-unreadable-object (chunk stream :type T)
        (format stream "~{~s~^ ~}" (make-args chunk)))))

(defmethod paint :around ((chunk chunk) painter)
  (when (visible chunk (q+:viewport painter))
    (call-next-method)))

(defun list-real-chunks ()
  (let ((classes ()))
    (labels ((scan (class)
               (loop for class in (c2mop:class-direct-subclasses class)
                     do (scan class)
                        (unless (c2mop:subclassp class (find-class 'virtual-chunk))
                          (push class classes)))))
      (scan (find-class 'real-chunk)))
    classes))

;;;;;
;; Chunk forms
(defclass square-chunk (chunk)
  ((size :initarg :size :initform 32 :accessor size)))

(defmethod (cl:setf size) (size (square square-chunk))
  (setf (slot-value square 'size) size
        (w square) size
        (h square) size
        (top square) (/ size 2)
        (left square) (/ size 2)))

(defmethod initialize-instance :after ((square square-chunk) &key)
  (setf (size square) (size square)))

(defmethod make-args ((chunk square-chunk))
  `(:size ,(size chunk)
    ,@(when (next-method-p)
        (call-next-method))))

(defclass rectangular-chunk (chunk)
  ())

(defmethod paint ((chunk rectangular-chunk) painter)
  (q+:draw-rect painter
                (floor (- (x chunk) (left chunk)))
                (floor (- (y chunk) (top chunk)))
                (floor (w chunk)) (floor (h chunk))))

(defclass circular-chunk (chunk)
  ())

(defmethod paint ((chunk circular-chunk) painter)
  (q+:draw-ellipse painter
                   (floor (- (x chunk) (left chunk)))
                   (floor (- (y chunk) (top chunk)))
                   (floor (w chunk)) (floor (h chunk))))
;;;;;
;; Chunk visuals
(defclass colored-chunk (chunk)
  ((color :initarg :color :initform (error "COLOR required.") :accessor color)))

(defmethod paint :before ((chunk colored-chunk) painter)
  (setf (q+:pen painter) (q+:qt.no-pen))
  (setf (q+:brush painter) (q+:make-qbrush (color chunk) (q+:qt.solid-pattern))))

(defclass textured-chunk (plain-chunk)
  ((texture :initarg :texture :initform (error "TEXTURE required.") :accessor texture)))

(defmethod initialize-instance :after ((chunk textured-chunk) &key)
  (setf (texture chunk)
        (etypecase (texture chunk)
          (pathname (q+:make-qimage (uiop:native-namestring (texture chunk))))
          (string (q+:make-qimage (texture chunk)))
          (qobject (texture chunk)))))

(defmethod paint :before ((chunk textured-chunk) painter)
  (setf (q+:pen painter) (q+:qt.no-pen))
  (setf (q+:brush painter) (q+:make-qbrush (texture chunk))))

(defclass white-square (rectangular-chunk square-chunk colored-chunk real-chunk)
  ()
  (:default-initargs :color (q+:qt.white)))

(defclass red-ball (circular-chunk square-chunk colored-chunk real-chunk)
  ()
  (:default-initargs :color (q+:qt.red)))
