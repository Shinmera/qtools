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
  ;; copying.lisp
  (:export
   #:copy-using-class
   #:copy
   #:define-copy-method
   #:describe-copy-method)
  ;; finalizable.lisp
  (:export
   #:finalizable-class
   #:finalizable-slot
   #:finalized
   #:finalizable
   #:define-finalizable
   #:finalize-using-class
   #:finalize
   #:define-finalize-method
   #:describe-finalize-method
   #:with-finalizing
   #:with-finalizing*)
  ;; gc-finalized.lisp
  (:export
   #:gc-finalized
   #:unbox
   #:make-gc-finalized
   #:with-gc-finalized)
  ;; keychord-editor.lisp
  (:export
   #:keychord-editor)
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
   #:value
   #:qobject-alive-p
   #:maybe-delete-qobject
   #:qtenumcase
   #:clear-layout
   #:enumerate-method-descriptors
   #:ensure-class
   #:with-slots-bound
   #:fuse-alists
   #:fuse-plists
   #:split
   #:capitalize-on)
  ;; widget.lisp
  (:export
   #:widget-class
   #:widget-class-direct-options
   #:widget-class-extern-options
   #:widget-class-initializers
   #:widget-class-finalizers
   #:widget
   #:call-initializers
   #:call-finalizers
   #:softly-redefine-widget-class
   #:set-widget-class-option
   #:remove-widget-class-option
   #:define-widget)
  ;; widget-convenience.lisp
  (:export
   #:define-slot
   #:define-override
   #:define-initializer
   #:define-finalizer
   #:define-signal
   #:define-subwidget)
  ;; widget-defmethod.lisp
  (:shadow
   #:defmethod)
  (:export
   #:*method*
   #:method-declaration
   #:remove-method-declaration
   #:define-method-declaration
   #:defmethod
   #:slot
   #:override
   #:initializer
   #:finalizer))

;; hack to make defmethod shadowing convenient
(setf (macro-function 'qtools:defmethod)
      (macro-function 'cl:defmethod))
