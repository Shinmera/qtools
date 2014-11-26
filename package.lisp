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
  ;; environment.lisp
  (:export
   #:*environment-widget-forms*
   #:environment-form
   #:remove-environment-form
   #:define-environment-form
   #:define-environment-form-class-option
   #:list-environment-forms
   #:describe-environment-form
   #:declare-environment-widget-form
   #:undeclare-environment-widget-form
   #:with-widget-environment)
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
  ;; widget-menu.lisp
  (:export
   #:*menu*
   #:*widget*
   #:*class*
   #:actions
   #:add-action
   #:menu-option
   #:remove-menu-option
   #:define-menu-option
   #:call-menu-option
   #:make-chord
   #:make-action)
  ;; widget-options.lisp
  (:export
   #:*widget-init-priority*
   #:*slot-init-priority*
   #:*layout-init-priority*)
  ;; widget.lisp
  (:export
   #:widget-class
   #:add-initializer
   #:add-finalizer
   #:process-widget-class-option
   #:widget-class-option
   #:list-widget-class-options
   #:remove-widget-class-option
   #:define-widget-class-option
   #:widget-slot-option
   #:list-widget-slot-options
   #:remove-widget-slot-otpion
   #:process-widget-slot-option
   #:define-widget-slot-option
   #:describe-widget-option
   #:widget
   #:define-widget
   #:make-widget))
