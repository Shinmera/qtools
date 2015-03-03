#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.game)
(named-readtables:in-readtable :qtools)

(defclass point ()
  ((x :initarg :x :initform 0 :accessor x)
   (y :initarg :y :initform 0 :accessor y)))

(defun point (x y)
  (make-instance 'point :x x :y y))

(defclass rectangle ()
  ((w :initarg :w :initform 0 :accessor w)
   (h :initarg :h :initform 0 :accessor h)))

(defclass positioned-rectangle (point rectangle)
  ((top :initarg :top :initform 0 :accessor top)
   (left :initarg :left :initform 0 :accessor left)))

(defgeneric paint (thing painter)
  (:method ((point point) painter)
    (q+:draw-point painter (x point) (y point)))
  (:method ((rect rectangle) painter)
    (q+:draw-rect painter 0 0 (w rect) (h rect)))
  (:method ((rect positioned-rectangle) painter)
    (q+:draw-rect painter (- (x rect) (left rect)) (- (y rect) (top rect)) (w rect) (h rect))))

(defgeneric visible (thing qrect)
  (:method ((point point) qrect)
    (and (<= (q+:left qrect) (x point) (q+:right qrect))
         (<= (q+:top qrect) (y point) (q+:bottom qrect))))
  (:method ((rect positioned-rectangle) qrect)
    (and (or (<= (q+:left qrect) (- (x rect) (left rect)) (q+:right qrect))
             (<= (q+:left qrect) (+ (- (x rect) (left rect)) (w rect)) (q+:right qrect)))
         (or (<= (q+:top qrect) (- (y rect) (top rect)) (q+:bottom qrect))
             (<= (q+:top qrect) (+ (- (y rect) (top rect)) (h rect)) (q+:bottom qrect)))))
  (:method ((rect positioned-rectangle) (point point))
    (and (<= (- (x rect) (left rect)) (x point) (+ (- (x rect) (left rect)) (w rect)))
         (<= (- (y rect) (top rect)) (y point) (+ (- (y rect) (top rect)) (h rect))))))

(defgeneric make-args (thing)
  (:method ((point point))
    `(:x ,(x point)
      :y ,(y point)
      ,@(when (next-method-p)
          (call-next-method))))
  (:method ((rect rectangle))
    `(:w ,(w rect)
      :h ,(h rect)
      ,@(when (next-method-p)
          (call-next-method))))
  (:method ((rect positioned-rectangle))
    `(:top ,(top rect)
      :left ,(left rect)
      ,@(when (next-method-p)
          (call-next-method)))))
