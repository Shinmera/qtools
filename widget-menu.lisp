#|
This file is a part of QTools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *actions* (make-hash-table))
(defvar *menu-options* (make-hash-table))

(defun actions (&optional class)
  (if class
      (gethash (class-name (ensure-class class)) *actions*)
      (loop for actions being the hash-values of *actions*
            append actions)))

(defun (setf actions) (list class)
  (setf (gethash (class-name (ensure-class class)) *actions*)
        list))

(defun add-action (action class)
  (let ((pos (position (#_text action) (actions class) :key #'(lambda (o) (#_text o)) :test #'string-equal)))
    (if pos
        (setf (nth pos (actions class)) action)
        (push action (actions class)))))

(defun prune-deleted-actions (class)
  (setf (actions class)
        (delete-if #'qt:qobject-deleted (actions class))))

(defun menu-option (name)
  (gethash name *menu-options*))

(defun (setf menu-option) (func name)
  (setf (gethash name *menu-options*)
        func))

(defun remove-menu-option (name)
  (remhash name *menu-options*))

(defmacro define-menu-option (name args &body body)
  `(setf (menu-option ,name)
         #'(lambda (,@args)
             ,@body)))

(defun call-menu-option (name body)
  (apply (or (menu-option name)
             (error "No such menu option ~s" name))
         body))

(defun make-chord (chord)
  (etypecase chord
    (null NIL)
    (list (format NIL "~{~@(~a~)~^+~}" chord))
    (symbol (capitalize-on #\- chord #\+ T))
    (string chord)))

(defun make-action (widget menu name &key slot keychord)
  (let ((item (#_new QAction (capitalize-on #\- name #\Space T) menu)))
    (when keychord
      (#_setShortcut item (#_new QKeySequence (make-chord keychord))))
    (when slot
      (connect item "triggered()" widget slot))
    (add-action item widget)
    (#_addAction menu item)
    item))

(defvar *menu*)
(defvar *widget*)
(defvar *class*)

(define-menu-option :item (name &rest body)
  (destructuring-bind (name &optional keychord) (if (listp name) name (list name))
    (let ((slot (specified-type-method-name (make-symbol (format NIL "MENU-ITEM-~:(~a~)" name)) ())))
      (values
       `((make-action ,*widget* ,*menu* ',name :slot ,slot :keychord ,(make-chord keychord)))
       `(:slots ((,slot
                  (lambda (widget)
                    (with-slots-bound (widget ,*class*)
                      ,@body)))))))))

(define-menu-option :separator ()
  `((#_addSeparator ,*menu*)))

(define-menu-option :slot-menu (slot)
  `((#_addMenu ,*menu* (slot-value ,*widget* ',slot))))

(define-menu-option :menu (name &rest forms)
  (let* ((menu (gensym "MENU"))
         (outer *menu*)
         (*menu* menu))
    (loop for (type . body) in forms
          for (forms options) = (multiple-value-list (call-menu-option type body))
          appending forms into all-forms
          appending options into all-options
          finally (return
                    (values
                     `((let ((,menu (#_new QMenu ,(capitalize-on #\- name #\Space T) ,outer)))
                         (#_addMenu ,outer ,menu)
                         ,@all-forms
                         ,menu))
                     all-options)))))

(define-widget-class-option :menus (class name &rest items)
  ""
  (let ((*widget* (gensym "WIDGET"))
        (*menu* (gensym "BAR"))
        (*class* class))
    (multiple-value-bind (forms options) (call-menu-option :menu (list* name items))
      (add-initializer
       class 50
       `(lambda (,*widget*)
          (prune-deleted-actions ,class)
          (let ((,*menu* (#_menuBar ,*widget*)))
            ,@forms)))
      options)))
