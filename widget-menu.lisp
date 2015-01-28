#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *widget*)
(defvar *menu-content-types* (make-hash-table :test 'eql))
(defvar *widget-actions* (make-hash-table :test 'eql))

(defun widget-actions (class)
  (gethash (class-name (ensure-class class)) *widget-actions*))

(defun (setf widget-actions) (actions class)
  (setf (gethash (class-name (ensure-class class)) *widget-actions*)
        actions))

(defun menu-content-type (name)
  (gethash name *menu-content-types*))

(defun (setf menu-content-type) (function name)
  (setf (gethash name *menu-content-types*) function))

(defun remove-menu-content-type (name)
  (remhash name *menu-content-types*))

(defmacro define-menu-content-type (type (parent &rest args) &body body)
  `(setf (menu-content-type ',type)
         #'(lambda (,parent ,@args)
             ,@body)))

(defun build-menu-content (parent type contents)
  (apply (or (menu-content-type type)
             (error "No such menu content type ~s" type))
         parent contents))

(defun make-chord (chord)
  "Transforms CHORD into a keychord string, if possible."
  (etypecase chord
    (null NIL)
    (list (format NIL "~{~@(~a~)~^+~}" chord))
    (symbol (capitalize-on #\- chord #\+ T))
    (string chord)))

(define-menu-content-type :menu (parent identifier &rest contents)
  (let ((menu (etypecase identifier
                (symbol (slot-value *widget* identifier))
                (string (#_new QMenu identifier parent)))))
    (#_addMenu parent menu)
    (loop for (type . content) in contents
          do (build-menu-content menu type content))
    menu))

(define-menu-content-type :separator (parent)
  (#_addSeparator parent)
  NIL)

(define-menu-content-type :item (parent identifier &rest body)
  (let ((item (etypecase identifier
                (symbol (slot-value *widget* identifier))
                (string (#_new QAction identifier parent))
                (list (destructuring-bind (identifier keychord) identifier
                        (let ((item (#_new QAction identifier parent)))
                          (#_setShortcut item (#_new QKeySequence (make-chord keychord)))
                          item))))))
    (#_addAction parent item)
    (push item (widget-actions *widget*))
    (when body
      (let ((slot (format NIL "m_~aActionSlot()" (remove-if #'(lambda (c) (find c "abcdefghijklmonpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")) (#_text item)))))
        (set-widget-class-option *widget* :slots `(,slot
                                                   (lambda (widget)
                                                     (with-slots-bound (widget ,(class-name (class-of *widget*)))
                                                       ,@body))))
        (connect item "triggered()" *widget* slot)))
    item))

(defmacro define-menu ((widget-class name) &body contents)
  (let ((initfunc (intern (format NIL "%BUILD-~a-MENU-~a" widget-class name) *package*)))
    `(define-initializer (,widget-class ,initfunc 9)
       (let ((*widget* ,widget-class))
         (setf (widget-actions *widget*) ())
         (build-menu-content (#_menuBar ,widget-class) :menu '(,(capitalize-on #\- name #\Space T) ,@contents))))))
