#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:qtools-opengl
  (:nicknames #:org.shirakumo.qtools.opengl)
  (:use #:cl+qt)
  (:export #:main))
(in-package #:org.shirakumo.qtools.opengl)
(named-readtables:in-readtable :qtools)

(define-widget opengl (QGLWidget)
  ((angle :initform 0)
   (angle-delta :initform 1)))

(define-subwidget (opengl timer) (q+:make-qtimer opengl)
  (setf (q+:single-shot timer) NIL)
  (q+:start timer (round 1000/30)))

(define-slot (opengl update) ()
  (declare (connected timer (timeout)))
  (incf angle angle-delta)
  (q+:repaint opengl))

(define-initializer (opengl setup)
  (setf (q+:window-title opengl) "Qtools OpenGL Example")
  (setf (q+:fixed-size opengl) (values 500 500)))

(define-override (opengl key-release-event) (ev)
  (cond ((= (q+:key ev) (q+:qt.key_left))
         (decf angle-delta))
        ((= (q+:key ev) (q+:qt.key_right))
         (incf angle-delta)))
  (stop-overriding))

(define-override (opengl paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter opengl)))
    (q+:begin-native-painting painter)

    (gl:push-matrix)
    (gl:translate 250 250 0)
    (gl:rotate angle 0 0 1)
    (gl:with-primitives :quads
      (gl:color 1 0 0)
      (gl:vertex -50 -50)
      (gl:color 0 1 0)
      (gl:vertex 50 -50)
      (gl:color 0 0 1)
      (gl:vertex 50 50)
      (gl:color 1 1 1)
      (gl:vertex -50 50))
    (gl:pop-matrix)

    (q+:end-native-painting painter)
    (with-finalizing ((font (q+:make-qfont "Monospace" 10)))
      (setf (q+:style-hint font) (q+:qfont.type-writer))
      (setf (q+:font painter) font)
      (q+:draw-text painter 5 15 "<left>:  Decrease speed")
      (q+:draw-text painter 5 30 "<right>: Increase speed"))))

(defun main ()
  (with-main-window (window (make-instance 'opengl))))
