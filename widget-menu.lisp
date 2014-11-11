#|
This file is a part of QTools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(defvar *actions* (make-hash-table))
(defvar *menu-options* (make-hash-table))
(defvar *menu*)
(defvar *widget*)
(defvar *class*)
(setf (documentation '*menu* 'variable) "Bound to the symbol that is bound to the current QMenu during evaluation.")
(setf (documentation '*widget* 'variable) "Bound to the symbol that is bound to the widget class instance during evaluation.")
(setf (documentation '*class* 'variable) "Bound to the current class the menu is being expanded for.")

(defun actions (&optional class)
  "Returns a fresh list of QActions that belong to the specified CLASS.
If no CLASS is specified, all actions are returned."
  (if class
      (remove-if #'qt:qobject-deleted (gethash (class-name (ensure-class class)) *actions*))
      (loop for actions being the hash-values of *actions*
            append (remove-if #'qt:qobject-deleted actions))))

(defun (setf actions) (list class)
  "Sets the list of actions for the specified class."
  (setf (gethash (class-name (ensure-class class)) *actions*)
        list))

(defun add-action (action class)
  "Adds the ACTION to the CLASS' list of actions.
If an action with a string-equal #_text already exists, it is replaced."
  (let ((pos (position (#_text action) (actions class) :key #'(lambda (o) (#_text o)) :test #'string-equal)))
    (if pos
        (setf (nth pos (actions class)) action)
        (push action (actions class)))))

(defun prune-deleted-actions (class)
  "Removes all deleted action objects from the CLASS' list of actions."
  (setf (actions class)
        (delete-if #'qt:qobject-deleted (actions class))))

(defun menu-option (name)
  "Returns the menu option expander for NAME."
  (gethash name *menu-options*))

(defun (setf menu-option) (func name)
  "Sets FUNC as the menu option expander for NAME."
  (setf (gethash name *menu-options*)
        func))

(defun remove-menu-option (name)
  "REmoves the menu option NAME."
  (remhash name *menu-options*))

(defmacro define-menu-option (name args &body body)
  "Defines a new menu option of NAME that destructures its body according to ARGS.
The BODY should return two values, the first being a list of forms to expand into.
The second should be a plist of class-options to add. Since the expansion of menu
options happens during the class-option expansion step of a widget definition, you
cannot use the second value to add new class-slots.

See widget-class-option :MENU
See DEFINE-WIDGET-CLASS-OPTION
See MAKE-CHORD
See MAKE-ACTION
See *WIDGET*
See *MENU*
See *CLASS*"
  `(setf (menu-option ,name)
         #'(lambda (,@args)
             ,@body)))

(defun call-menu-option (name body)
  "Calls the menu option NAME with BODY.
Returns the expansion of the option or signals an error if no such option exists.

See DEFINE-MENU-OPTION"
  (apply (or (menu-option name)
             (error "No such menu option ~s" name))
         body))

(defun make-chord (chord)
  "Transforms CHORD into a keychord string, if possible."
  (etypecase chord
    (null NIL)
    (list (format NIL "~{~@(~a~)~^+~}" chord))
    (symbol (capitalize-on #\- chord #\+ T))
    (string chord)))

(defun make-action (widget menu name &key slot keychord)
  "Creates a new QAction on WIDGET's MENU with name NAME.
If SLOT is specified, the action is CONNECTed to the WIDGET's SLOT.
If KEYCHORD is specified, it is parsed using MAKE-CHORD and set as  the
action's key sequence."
  (let ((item (#_new QAction (capitalize-on #\- name #\Space T) menu)))
    (when keychord
      (#_setShortcut item (#_new QKeySequence (make-chord keychord))))
    (when slot
      (connect item "triggered()" widget slot))
    (add-action item widget)
    (#_addAction menu item)
    item))

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

(define-widget-class-option :menus (class name &rest options)
  "Defines a menu on the class called NAME.
The body should be a set of options, like so:

 (my-menu
   (:item Foo)
   (:menu Sub-Menu
     (:item Some-Other-Item)))

What options are available can be changed using DEFINE-MENU-OPTION.
By default the following options are available:

  (:ITEM name form*)
    NAME ::= name | (name keychord)
    Creates a new menu item with NAME and optional KEYCHORD.
    When the item is clicked, the body is executed.
    In order to make the automatic body possible, a slot on the widget
    is generated for each menu item. The name for each slot is
    menuItemNAME (where NAME is the capitalised version of the specifed
    name). 
    
  (:MENU name options*)
    Defines a sub-menu with NAME. Works identical to the toplevel
    DEFINE-MENU form.

  (:SLOT-MENU slot)
    Uses the SLOT symbol as a sub-menu. This is useful for defining
    dynamic menus.

  (:SEPARATOR)
    Adds a separator item.

See DEFINE-MENU-OPTION"
  (let ((*widget* (gensym "WIDGET"))
        (*menu* (gensym "BAR"))
        (*class* class))
    (multiple-value-bind (forms class-options) (call-menu-option :menu (list* name options))
      (add-initializer
       class 50
       `(lambda (,*widget*)
          (prune-deleted-actions ,class)
          (let ((,*menu* (#_menuBar ,*widget*)))
            ,@forms)))
      class-options)))
