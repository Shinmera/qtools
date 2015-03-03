#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.game)
(named-readtables:in-readtable :qtools)

(define-widget view (QGLWidget)
  ((game :initarg :game :initform (error "GAME required."))))

(define-override (view paint-event paint) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter view)))
    (setf (q+:background painter) (q+:make-qbrush (q+:qt.black)))
    (q+:erase-rect painter (q+:rect view))
    (paint (world game) painter)))

(define-override (view key-press-event key-press) (ev)
  (signal! game (key-press int) (q+:key ev)))

(define-override (view key-release-event key-release) (ev)
  (signal! game (key-release int) (q+:key ev)))

(define-override (view mouse-press-event mouse-press) (ev)
  (signal! game (mouse-press int int int) (q+:x ev) (q+:y ev) (q+:button ev)))

(define-override (view mouse-release-event mouse-release) (ev)
  (signal! game (mouse-release int int int) (q+:x ev) (q+:y ev) (q+:button ev)))

(define-override (view mouse-move-event mouse-move) (ev)
  (signal! game (mouse-move int int) (q+:x ev) (q+:y ev)))

(define-initializer (view setup)
  (setf (q+:size-policy view) (values (q+:qsizepolicy.minimum) (q+:qsizepolicy.minimum))))

(define-widget game (QWidget)
  ((world :initform (make-instance 'world) :accessor world)
   (framestep :initform (floor (/ 1000 30)))))

(define-signal (game key-press) (int))
(define-signal (game key-release) (int))
(define-signal (game mouse-press) (int int int))
(define-signal (game mouse-release) (int int int))
(define-signal (game mouse-move) (int int))

(define-subwidget (game timer) (q+:make-qtimer game)
  (setf (q+:single-shot timer) T))

(define-subwidget (game view) (make-instance 'view :game game))

(define-subwidget (game editor) (make-instance 'editor :game game))

(define-subwidget (game layout) (q+:make-qhboxlayout game)
  (q+:add-widget layout view)
  (q+:add-widget layout editor))

(defun get-internal-real-time-milis ()
  (/ (get-internal-real-time)
     (/ internal-time-units-per-second
        1000)))

(define-slot (game tick) ()
  (declare (connected timer (timeout)))
  (let ((start (get-internal-real-time-milis)))
    (q+:repaint view)
    (update T world)
    (q+:start timer (floor (max 0 (- framestep
                                     (- start (get-internal-real-time-milis))))))))

(define-initializer (game setup)
  (q+:start timer framestep))

(defun main ()
  (with-main-window (window (make-instance 'game))))
