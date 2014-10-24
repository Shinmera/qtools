#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defclass finalizable-qt-class (finalizable-class qt-class)
  ())

(defclass qt-widget (finalizable)
  ()
  (:metaclass finalizable-qt-class))

(defmacro define-qt-widget (name (qt-class &rest direct-superclasses) direct-slots &rest options)
  `(defclass ,name (qt-widget ,@direct-superclasses)
     ,direct-slots
     (:metaclass finalizable-qt-class)
     (:qt-superclass ,(find-qt-class-name qt-class))
     ,@options))

(indent:define-indentation define-qt-widget (4 (&whole 6 &rest) (&whole 2 (&whole 0 0 &rest 2)) &rest (&whole 2 2 4 &body)))

(defmacro define-qt-complex (&body options)
  )

#+#:this-is-a-sample-experiment-to-look-at-the-potential-syntax
(define-qt-complex  
  (define-qt-widget ex-slider-widget (QWidget)
    ((max :initform 100 :initarg :max :accessor exs-max)
     (min :initform 0 :initarg :min :accessor exs-min)
     (step :initform 1 :initarg :step :accessor exs-step)
     (default :initform 0 :initarg :default :accessor exs-default)
     (divisor :initform 1 :initarg :divisor :accessor exs-divisor))
    
    (:widget slider (#_new QSlider (#_Qt::Horizontal))
      (#_setTickInterval slider (round (* divisor step)))
      (#_setMaximum slider (round (* divisor max)))
      (#_setMinimum slider (round (* divisor min)))
      (#_setValue slider (round (* divisor default))))
    
    (:widget spin-box (if (= 1 divisor) (#_new QSpinBox) (#_new QDoubleSpinBox))
      (#_setSingleStep spin-box step)
      (#_setMaximum spin-box max)
      (#_setMinimum spin-box min)
      (#_setValue spin-box default)
      (#_setFixedWidth spin-box 70))
    
    (:widget button (#_new QPushButton)
      (#_setText button (princ-to-string default))
      (#_setFixedWidth button 50))

    (:layout layout (#_new QHBoxLayout)
      (#_setSpacing layout 0)
      (#_setContentsMargins layout 0 0 0 0)
      (#_addWidget layout slider 8)
      (#_addWidget layout spin-box 1)
      (#_addWidget layout button 1)))

  (define-signal value-changed ((double int)))
  (define-signal on-release ((double int)))

  (define-slot update (widget (value int double))
    (declare (connected slider (value-changed int)))
    (declare (connected spin-box (value-changed double)))
    (when (or (/= (#_value slider) value)
              (/= (#_value spin-box) value))
      (if (= 1 divisor)
          (progn
            (when (and (= (#_value spin-box) value)
                       (/= (#_value slider) value))
              (emit-signal widget "onRelease(int)" value))
            (#_setValue slider value)
            (#_setValue spin-box value)
            (emit-signal widget "valueChanged(int)" value))
          (progn
            (when (and (= (#_value spin-box) value)
                       (/= (#_value slider) value))
              (emit-signal widget "onRelease(double)" value))
            (when (= (#_value slider) value)
              (setf value (/ value divisor)))
            (#_setValue slider (round (* value divisor)))
            (#_setValue spin-box value)
            (emit-signal widget "valueChanged(double)" value)))))

  (define-slot release (widget)
    (declare (connected slider (slider-released)))
    (declare (connected button (clicked)))
    (emit-signal widget "onRelease(int)" (#_value slider)))

  (define-slot reset (widget)
    (declare (connected button (clicked)))
    (#_setValue slider (round (* divisor default)))
    (#_setValue spin-box default)))
