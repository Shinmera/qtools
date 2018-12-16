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
  (etypecase chord
    (null NIL)
    (list (format NIL "~{~@(~a~)~^+~}" chord))
    (symbol (capitalize-on #\- chord #\+ T))
    (string chord)))

(define-menu-content-type :menu (parent identifier &rest contents)
  (let ((menu (gensym "MENU"))
        (initforms)
        (side-forms))
    (loop for (type . content) in contents
          do (multiple-value-bind (initform side-form) (build-menu-content menu type content)
               (when initform (push initform initforms))
               (when side-form (push side-form side-forms))))
    (values
     `(let ((,menu ,(etypecase identifier
                      (symbol `(slot-value *widget* ',identifier))
                      (string `(#_new QMenu ,identifier ,parent)))))
        (#_addMenu ,parent ,menu)
        ,@(nreverse initforms))
     (when side-forms
       `(progn ,@(nreverse side-forms))))))

(define-menu-content-type :separator (parent)
  `(#_addSeparator ,parent))

(define-menu-content-type :item (parent identifier &rest body)
  (let ((slot (format NIL "m_~aActionSlot()" (remove-if-not #'(lambda (c) (find c "abcdefghijklmonpqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                                                            (princ-to-string identifier))))
        (item (gensym "ITEM")))
    (values
     `(let ((,item ,(etypecase identifier
                      (symbol `(slot-value *widget* ',identifier))
                      (string `(#_new QAction ,identifier ,parent))
                      (list `(#_new QAction ,(first identifier) ,parent)))))
        ,@(when (listp identifier)
            `((#_setShortcut ,item (#_new QKeySequence ,(make-chord (second identifier))))))
        (#_addAction ,parent ,item)
        (push ,item (widget-actions *widget*))
        ,@(when body
            `((connect ,item "triggered()" *widget* ,slot))))
     (when body
       `(set-widget-class-option ',*widget* :slots '(,slot
                                                    (lambda (,*widget*)
                                                      (with-slots-bound (,*widget* ,*widget*)
                                                        ,@body))))))))

(defmacro define-menu ((widget-class name) &body contents)
  (let* ((*print-case* (readtable-case *readtable*))
         (initfunc (intern (format NIL "~a-~a-~a-~a" '%build widget-class 'menu name) *package*))
         (menu (gensym "MENU-BAR"))
         (*widget* widget-class))
    (multiple-value-bind (initform side-form) (build-menu-content menu :menu `(,(capitalize-on #\- name #\Space T) ,@contents))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,side-form
         (define-initializer (,widget-class ,initfunc 9)
           (let* ((*widget* ,widget-class)
                  (,menu (#_menuBar *widget*)))
             (setf (widget-actions *widget*) ())
             ,initform))))))
