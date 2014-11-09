#|
This file is a part of QTools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *actions* ())

;; Fixme: Doesn't work atm as Qtools widgets don't handle
;; C++ init args...
(with-widget-environment
  (define-widget keychord-editor ("QDialog")
    ((old-accelerator :initform "")))

  (define-subwidget action-table (#_new QTableWidget (length *actions*) 2)
    (connect! action-table (current-changed int int) widget (record-action int int))
    (connect! action-table (value-changed int int) widget (validate-action int int))
    
    (#_setText (#_horizontalHeaderItem action-table 0) "Item")
    (#_setText (#_horizontalHeaderItem action-table 1) "Keychord")
    (#_setLeftMargin action-table 0)
    (#_setColumnReadOnly action-table 0 T)
    (loop for row from 0
          for action in *actions*
          do (#_setText action-table row 0 (#_text action))
             (#_setText action-table row 1 (#_new QString (#_accel action)))))

  (define-subwidget ok-button (#_new QPushButton "&Ok")
    (connect! ok-button (clicked) widget (accept)))
  
  (define-subwidget cancel-button (#_new QPushButton "&Cancel")
    (connect! cancel-button (clicked) widget (reject)))

  (define-subwidget button-layout (#_new QHBoxLayout)
    (#_setSpacing button-layout 8)
    (#_addStretch button-layout 8)
    (#_addWidget button-layout ok-button)
    (#_addWidget button-layout cancel-button))

  (define-layout layout (#_new QVBoxLayout)
    (#_setMargin layout 8)
    (#_setSpacing layout 8)
    (#_addWidget layout action-table)
    (#_addLayout layout button-layout))

  (define-override accept (widget)
    (loop for row from 0
          for action in *actions*
          do (#_setAccel action (#_new QKeySequence (#_text action-table row 1))))
    (#_QDialog::accept))

  (define-slot record-action (widget (row int) (column int))
    (setf old-accelerator (#_text (#_item action-table row column))))

  (define-slot validate-action (widget (row int) (column int))
    (let* ((item (#_item action-table row column))
           (text (#_new QString (#_new QKeySequence (#_text item)))))
      (if (and (#_isEmpty text)
               (not (#_isEmpty (#_text item))))
          (#_setText item old-accelerator)
          (#_setText item text)))))

(define-widget-class-option :menus (class name &rest items)
  ""
  (let ((slots ())
        (widget (gensym "WIDGET"))
        (menu (gensym "MENU"))
        (item (gensym "ITEM")))
    (labels ((make-label (name &optional (delim #\Space))
               (with-output-to-string (stream)
                 (loop with capitalize = T
                       for char across (string-downcase name)
                       do (cond ((char= char #\-)
                                 (setf capitalize T)
                                 (write-char delim stream))
                                (capitalize
                                 (write-char (char-upcase char) stream)
                                 (setf capitalize NIL))
                                (T
                                 (write-char char stream))))))
             (make-chord (chord)
               (etypecase chord
                 (list (format NIL "~{~@(~a~)~^+~}" chord))
                 (symbol (make-label chord #\+))
                 (string chord)))
             (make-menu (name &rest forms)
               `(let ((,menu (#_new QMenu ,(make-label name))))
                  ,@(loop for (type . body) in forms
                          collect (ecase type
                                    (:item `(#_addAction ,menu ,(apply #'make-item body)))
                                    (:menu `(#_addMenu ,menu ,(apply #'make-menu body)))
                                    (:slot-menu `(#_addMenu ,menu (slot-value ,widget ',(car body))))
                                    (:separator `(#_addSeparator ,menu))))
                  ,menu))
             (make-slot (name &rest body)
               (let ((name (make-symbol (format NIL "MENU-ITEM-~:(~a~)" name))))
                 (push `(,(specified-type-method-name name ())
                         (lambda (widget)
                           (declare (ignorable widget))
                           ,@body))
                       slots)
                 name))
             (make-item (name &rest body)
               (destructuring-bind (name &optional keychord) (if (listp name) name (list name))
                 `(let ((,item (#_new QAction ,(make-label name) ,menu)))
                    ,@(when keychord
                        `((#_setShortcut ,item (#_new QKeySequence ,(make-chord keychord)))))
                    ,@(when body
                        `((connect! ,item (triggered) ,widget (,(apply #'make-slot name body)))))
                    (push ,item *actions*)
                    ,item))))
      (add-initializer
       class 50
       `(lambda (,widget)
          (#_addMenu
           (#_menuBar ,widget)
           ,(apply #'make-menu name items)))))
    
    `(:slots ,slots)))
