#|
This file is a part of QTools
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)
(named-readtables:in-readtable :qt)

(with-widget-environment
  (define-widget keychord-editor ("QDialog")
    ((old-accelerator :initform "")
     (keychord-class :initarg :class :initform NIL)))
  
  (define-subwidget action-table (#_new QTableWidget (length (actions)) 2)
    (connect! action-table (current-changed int int) widget (record-action int int))
    (connect! action-table (value-changed int int) widget (validate-action int int))
    
    (#_setHorizontalHeaderItem action-table 0 (#_new QTableWidgetItem "Item Name"))
    (#_setHorizontalHeaderItem action-table 1 (#_new QTableWidgetItem "Keychord"))
    (#_setVisible (#_verticalHeader action-table) NIL)
    (#_setResizeMode (#_horizontalHeader action-table) (#_QHeaderView::Stretch))
    (loop for row from 0
          for action in (actions keychord-class)
          for label = (#_new QTableWidgetItem (#_text action))
          for accel = (#_new QTableWidgetItem (#_toString (#_shortcut action)))
          do (#_setFlags label 0)
             (#_setItem action-table row 0 label)
             (#_setItem action-table row 1 accel)))

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
          for action in (actions keychord-class)
          do (#_setShortcut action (#_new QKeySequence (#_text (#_item action-table row 1)))))
    (stop-overriding))

  (define-slot record-action (widget (row int) (column int))
    (setf old-accelerator (#_text (#_item action-table row column))))

  (define-slot validate-action (widget (row int) (column int))
    (let* ((item (#_item action-table row column))
           (text (#_new QString (#_new QKeySequence (#_text item)))))
      (if (and (#_isEmpty text)
               (not (#_isEmpty (#_text item))))
          (#_setText item old-accelerator)
          (#_setText item text)))))
