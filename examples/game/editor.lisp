#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.game)
(named-readtables:in-readtable :qtools)

(define-widget chunk-widget (QWidget)
  ((chunk :initarg :chunk :initform (error "CHUNK required.") :accessor chunk)))

(define-override (chunk-widget paint-event) (ev)
  (declare (ignore ev))
  (with-finalizing ((painter (q+:make-qpainter chunk-widget)))
    (q+:translate painter (round (/ (q+:width chunk-widget) 2)) (round (/ (q+:height chunk-widget) 2)))
    (paint chunk painter)))

(define-widget chunk-selector (QTableWidget)
  ())

(define-initializer (chunk-selector setup)
  (setf (q+:size-policy chunk-selector) (values (q+:qsizepolicy.minimum) (q+:qsizepolicy.minimum)))
  (setf (q+:stretch-last-section (q+:horizontal-header chunk-selector)) T)
  (setf (q+:column-count chunk-selector) 1)
  (setf (q+:selection-mode chunk-selector) (q+:qabstractitemview.single-selection)) 
  (setf (q+:selection-behavior chunk-selector) (q+:qabstractitemview.select-rows))
  (q+:hide (q+:horizontal-header chunk-selector))
  (q+:hide (q+:vertical-header chunk-selector))
  (dolist (chunk (list-real-chunks))
    (q+:insert-row chunk-selector 0)
    (q+:set-cell-widget chunk-selector 0 0 (make-instance 'chunk-widget :chunk (make-instance chunk))))
  (q+:select-row chunk-selector 0))

(define-widget editor (QWidget)
  ((game :initarg :game :initform (error "GAME required."))
   (active :initarg :active :initform T :accessor active)
   (file :initarg :file :initform "" :accessor file)))

(define-subwidget (editor selector) (make-instance 'chunk-selector))

(define-subwidget (editor raster) (q+:make-qspinbox editor)
  (setf (q+:value raster) 32)
  (setf (q+:minimum raster) 1)
  (setf (q+:single-step raster) 1))

(define-subwidget (editor save) (q+:make-qpushbutton "Save" editor))

(define-subwidget (editor load) (q+:make-qpushbutton "Load" editor))

(define-subwidget (editor layout) (q+:make-qvboxlayout editor)
  (flet ((make-minlabel (name)
           (let ((label (q+:make-qlabel name editor)))
             (setf (q+:size-policy label) (values (q+:qsizepolicy.minimum) (q+:qsizepolicy.maximum)))
             label)))
    (q+:add-widget layout (make-minlabel "Chunks:"))
    (q+:add-widget layout selector)
    (q+:add-widget layout (make-minlabel "Raster:"))
    (q+:add-widget layout raster)
    (q+:add-widget layout save)
    (q+:add-widget layout load))
  (setf (q+:margin layout) 0)
  (setf (q+:spacing layout) 0)
  (setf (q+:size-policy editor) (values (q+:qsizepolicy.maximum) (q+:qsizepolicy.minimum))))

(defmethod chunk ((editor editor))
  (with-slots-bound (editor editor)
    (chunk (q+:cell-widget selector (q+:current-row selector) 0))))

(defun snap-to-raster (x editor)
  (let ((raster (q+:value (slot-value editor 'raster))))
    (* (round (/ x raster)) raster)))

(define-slot (editor mouse-release) ((x int) (y int) (button int))
  (declare (connected game (mouse-release int int int)))
  (cond ((= button (q+:qt.left-button))
         (add-chunk (make-instance (class-name (class-of (chunk editor)))
                                   :x (snap-to-raster x editor)
                                   :y (snap-to-raster y editor))
                    (world game)))
        ((= button (q+:qt.right-button))
         (remove-chunk (chunk-at (world game) x y)
                       (world game)))))

(define-slot (editor save) ()
  (declare (connected save (pressed)))
  (let ((new-file (q+:qfiledialog-get-save-file-name editor "Save World" file "Lisp Files (*.lisp-expr *.lisp)")))
    (when new-file
      (save-world (uiop:parse-native-namestring new-file) (world game) :if-exists :supersede)
      (setf file new-file))))

(define-slot (editor load) ()
  (declare (connected load (pressed)))
  (let ((new-file (q+:qfiledialog-get-open-file-name editor "Save World" file "Lisp Files (*.lisp-expr *.lisp)")))
    (when new-file
      (load-world (uiop:parse-native-namestring new-file) (world game))
      (setf file new-file))))
