#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.game)
(named-readtables:in-readtable :qtools)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass player (circular-chunk colored-chunk square-chunk rectangular-chunk real-chunk)
    ((vx :initform 0 :accessor vx)
     (vy :initform 0 :accessor vy)
     (vax :initform 1 :accessor vax)
     (vay :initform 10 :accessor vay)
     (vdx :initform -1.5 :accessor vdx)
     (vdy :initform -0.1 :accessor vdy)
     (vmx :initform 20 :accessor vmx)
     (vmy :initform 20 :accessor vmy)
     (facing :initform :right :accessor facing))
    (:default-initargs :color (q+:qt.blue))))

(defmethod paint ((player player) painter)
  (call-next-method))

(defmacro limitf (test var limit)
  `(when (,test ,limit ,var)
     (setf ,var ,limit)))

(defmethod update ((player player) (world world))
  (with-slots-bound (player player)
    (switch-key
     (:left
      (decf vx vax)
      (setf facing :left))
     (:right
      (incf vx vax)
      (setf facing :right))
     (:space
      (incf vy vay)))
    
    (limitf < vx vmx)
    (limitf > vx (- vmx))
    (limitf < vy vmy)
    (limitf > vy (- vmy))

    (when (and (no-key-p) (/= vx 0))
      (if (< vx 0) (decf vx vdx) (incf vx vdx))
      (when (< vdx vx (- vdx))
        (setf vx 0)))

    (incf (x player) vx)
    (incf (y player) vy)))
