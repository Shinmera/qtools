#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:qtools
  (:use #:cl #:qt)
  (:nicknames #:org.shirakumo.qtools)
  ;; class-map.lisp
  (:export
   #:*qt-class-vector*
   #:*qt-class-map*
   #:find-qt-class-name
   #:eqt-class-name)
  ;; copying.lisp
  (:export
   #:copy
   #:copy-qobject
   #:define-copy-method
   #:describe-copy-method)
  ;; dispatch.lisp
  (:export
   #:direct-qsubclass-p
   #:qclass-precedence-list
   #:dispatch-by-qclass
   #:qinstancep
   #:define-qclass-dispatch-function)
  ;; dynamic.lisp
  (:export
   #:ensure-q+-method
   #:q+
   #:q+fun
   #:fsetf)
  ;; finalizable.lisp
  (:export
   #:finalizable-class
   #:finalizable-slot
   #:finalized
   #:finalizable
   #:define-finalizable
   #:finalize
   #:finalize-qobject
   #:define-finalize-method
   #:describe-finalize-method
   #:with-finalizing
   #:with-finalizing*)
  (:shadow #:with-main-window)
  ;; gc-finalized.lisp
  (:export
   #:gc-finalized
   #:unbox
   #:make-gc-finalized
   #:with-gc-finalized)
  ;; generate.lisp
  (:export
   #:*target-package*
   #:*smoke-modules*
   #:*operator-map*
   #:*qmethods*
   #:*generated-modules*
   #:load-all-smoke-modules
   #:loaded-smoke-modules
   #:clear-method-info
   #:method-symbol
   #:process-method
   #:process-all-methods
   #:ensure-methods-processed
   #:compile-wrapper
   #:map-compile-all)
  ;; precompile.lisp
  (:export
   #:write-everything-to-file
   #:q+-compile-and-load
   #:smoke-module-system
   #:write-smoke-module-system-file)
  ;; printing.lisp
  (:export
   #:print-qobject
   #:define-print-method
   #:describe-print-method)
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
   #:parent
   #:qobject-alive-p
   #:maybe-delete-qobject
   #:qtenumcase
   #:qtypecase
   #:map-layout
   #:do-layout
   #:sweep-layout
   #:enumerate-method-descriptors
   #:find-children
   #:find-child
   #:ensure-qclass
   #:ensure-class
   #:with-slots-bound
   #:with-all-slots-bound
   #:fuse-alists
   #:fuse-plists
   #:split
   #:capitalize-on
   #:compilation-note
   #:emit-compilation-note
   #:default-application-name
   #:ensure-qapplication
   #:ensure-qobject
   #:with-main-window)
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
   #:widget-class-option-p
   #:set-widget-class-option
   #:remove-widget-class-option
   #:invalid-qt-superclass-hierarchy
   #:requested-qt-superclass
   #:clashing-qt-superclass
   #:clashing-superclass
   #:define-widget)
  ;; widget-convenience.lisp
  (:export
   #:define-slot
   #:define-override
   #:define-initializer
   #:define-finalizer
   #:define-signal
   #:define-subwidget
   #:remove-slot
   #:remove-override
   #:remove-initializer
   #:remove-finalizer
   #:remove-signal
   #:remove-subwidget)
  ;; widget-defmethod.lisp
  (:export
   #:*method*
   #:method-declaration
   #:with-widget-class
   #:remove-method-declaration
   #:define-method-declaration
   #:slot
   #:connected
   #:override
   #:initializer
   #:finalizer)
  ;; widget-menu
  (:export
   #:*widget*
   #:widget-actions
   #:menu-content-type
   #:remove-menu-content-type
   #:define-menu-content-type
   #:build-menu-content
   #:make-chord
   #:define-menu))

(defpackage #:cl+qt
  (:nicknames #:org.shirakumo.qtools+common-lisp)
  (:use #:cl #:qt #:qtools)
  (:import-from #:named-readtables #:in-readtable)
  (:shadowing-import-from #:qtools #:with-main-window)
  (:shadow #:setf #:defun #:defgeneric #:defmethod #:fdefinition #:function))

(do-symbols (symbol '#:cl+qt)
  (export (list symbol) '#:cl+qt))
