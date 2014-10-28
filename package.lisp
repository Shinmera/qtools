#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:qtools
  (:use #:cl #:qt)
  (:nicknames #:org.shirakumo.qtools)
  ;; class-map.lisp
  (:export
   #:*qt-class-vector*
   #:*qt-class-map*
   #:find-qt-class-name)
  ;; environment.lisp
  (:export
   #:environment-form
   #:define-environment-form
   #:define-environment-form-class-option
   #:with-widget-environment)
  ;; finalizable.lisp
  (:export
   #:finalizable-class
   #:finalizable-slot
   #:finalized
   #:finalizable
   #:define-finalizable
   #:finalize
   #:with-finalizing)
  ;; gc-finalized.lisp
  (:export
   #:gc-finalized
   #:unbox
   #:make-gc-finalized
   #:with-gc-finalized)
  ;; name-translation.lisp
  (:export
   #:to-method-name
   #:qt-type-of
   #:qt-type-for
   #:to-type-name
   #:cl-type-for
   #:eqt-type-of
   #:ecl-type-for
   #:determined-type-method-name
   #:specified-type-method-name)
  ;; signal.lisp
  (:export
   #:generic-signal
   #:signal!
   #:connect!
   #:define-signal-method)
  ;; toolkit.lisp
  (:export
   #:qobject-alive-p
   #:maybe-delete-qobject
   #:copy-qobject
   #:qtenumcase
   #:enumerate-method-descriptors)
  ;; widget.lisp
  (:export
   #:widget-class
   #:add-initializer
   #:process-widget-class-option
   #:define-widget-class-option
   #:process-widget-slot-option
   #:define-widget-slot-option
   #:widget
   #:define-widget))
