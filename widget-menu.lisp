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

(setf (documentation '*widget* 'variable)
      "Bound to the class-name of the widget during component expansion and
bound to the class instance during initialization.")

(defun widget-actions (class)
  "Returns a list of QAction instances that are active on the given CLASS."
  (gethash (class-name (ensure-class class)) *widget-actions*))

(defun (setf widget-actions) (actions class)
  "Sets the list of QAction instances that are active on the given CLASS."
  (setf (gethash (class-name (ensure-class class)) *widget-actions*)
        actions))

(defun menu-content-type (name)
  "Returns the function to process a menu content type NAME, if any."
  (gethash name *menu-content-types*))

(defun (setf menu-content-type) (function name)
  "Sets the FUNCTION to process menu contents of type NAME.

The function should accept in the very least one argument, which
is the symbol of the current parent. The other arguments can be 
used to decompose the remainder of the content form. Expected are
two return values, the first being a form to call during
initialization and the second being a form to be run alongside
the initializer definition."
  (setf (gethash name *menu-content-types*) function))

(defun remove-menu-content-type (name)
  "Removes the menu content type NAME."
  (remhash name *menu-content-types*))

(defmacro define-menu-content-type (type (parent &rest args) &body body)
  "Defines a new menu content type processor NAME.

See (SETF QTOOLS:MENU-CONTENT-TYPE)."
  `(setf (menu-content-type ',type)
         #'(lambda (,parent ,@args)
             ,@body)))

(defun build-menu-content (parent type contents)
  "Calls the appropriate function to parse menu content of TYPE.

See (SETF QTOOLS:MENU-CONTENT-TYPE)."
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
  "Defines a menu on WIDGET-CLASS with NAME and CONTENTS.

By default the following content types are available:
  A :MENU form is followed by a menu text string and a
  body of content forms.

  A :SEPARATOR simply adds a separator at its point to
  the parent and takes no further arguments.

  An :ITEM form is followed by an identifier, which may
  be a symbol, string, or list. In the case of a symbol,
  the item is taken from the according slot on the widget.
  In the case of a string the string serves as the text
  for the item. For a list, the first serves as the text
  and the second as an input acceptable to MAKE-CHORD.
  The body of the item form can be arbitrary lisp forms
  to be executed when the item is triggered.

See QTOOLS:MAKE-CHORD."
  (let ((initfunc (intern (format NIL "%BUILD-~a-MENU-~a" widget-class name) *package*))
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
