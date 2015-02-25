#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:titter
  (:nicknames #:org.shirakumo.qtools.titter)
  (:use #:cl+qt)
  (:export #:main))
(in-package #:titter)
(in-readtable :qtools)

;; Please have a look at the following blog post,
;; it explains the entire application in the form
;; of a simple tutorial.
;; https://reader.tymoon.eu/article/313

(defvar *logged-in* NIL)
(defvar *twitter-application-key* "D1pMCK17gI10bQ6orBPS0w")
(defvar *twitter-application-secret* "BfkvKNRRMoBPkEtDYAAOPW4s2G9U8Z7u3KAf0dBUA")

(defun set-url (widget)
  (let ((url (chirp:initiate-authentication
              :api-key *twitter-application-key*
              :api-secret *twitter-application-secret*)))
    (setf (q+:text widget) (format NIL "Please enter the pin from <a href=\"~a\">twitter</a>." url))))

(define-widget login (QDialog)
  ())

(define-subwidget (login url) (q+:make-qlabel login)
  (setf (q+:text-format url) (q+:qt.rich-text))
  (setf (q+:text-interaction-flags url) (q+:qt.text-browser-interaction))
  (setf (q+:open-external-links url) T)
  (set-url url))

(define-subwidget (login pin) (q+:make-qlineedit login)
  (setf (q+:placeholder-text pin) "PIN"))

(define-subwidget (login go) (q+:make-qpushbutton "Login!" login))

(define-subwidget (login layout) (q+:make-qvboxlayout login)
  (setf (q+:window-title login) "Login to Twitter")
  (q+:add-widget layout url)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner pin)
    (q+:add-widget inner go)
    (q+:add-layout layout inner)))

(define-slot (login done) ()
  (declare (connected go (released)))
  (setf (q+:cursor login) (q+:make-qcursor (q+:qt.wait-cursor)))
  (handler-case
      (chirp:complete-authentication (q+:text pin))
    (error (err)
      (declare (ignore err))
      (q+:qmessagebox-critical login "Error!" "Failed to login.")
      (setf (q+:text pin) "")
      (set-url url)
      (setf (q+:cursor login) (q+:make-qcursor (q+:qt.arrow-cursor))))
    (:no-error (&rest args)
      (declare (ignore args))
      (setf *logged-in* T)
      (q+:close login))))

(define-widget client (QWidget)
  ())

(define-subwidget (client status) (q+:make-qlineedit client)
  (setf (q+:placeholder-text status) "What's old?.."))

(define-subwidget (client tweet) (q+:make-qpushbutton "Tweet!" client))

(define-subwidget (client timeline) (q+:make-qlistwidget client)
  (setf (q+:word-wrap timeline) T)
  (setf (q+:text-elide-mode timeline) (q+:qt.elide-none)))

(define-subwidget (client layout) (q+:make-qvboxlayout client)
  (setf (q+:window-title client) "Titter")
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner status)
    (q+:add-widget inner tweet)
    (q+:add-layout layout inner))
  (q+:add-widget layout timeline))

(define-slot (client tweet) ()
  (declare (connected tweet (released)))
  (cond ((<= 1 (chirp:compute-status-length (q+:text status)) 140)
         (setf (q+:cursor client) (q+:make-qcursor (q+:qt.wait-cursor)))
         (handler-case
             (chirp:statuses/update (q+:text status))
           (error (err)
             (q+:qmessagebox-critical client "Error!" (format NIL "Failed to tweet: ~a" err)))
           (:no-error (&rest args)
             (declare (ignore args))
             (setf (q+:text status) "")))
         (setf (q+:cursor client) (q+:make-qcursor (q+:qt.arrow-cursor))))
        (T
         (q+:qmessagebox-information client "Huh?" "Tweet must be between 1 and 140 characters long!"))))

(define-signal (client new-tweet) (string string))

(define-slot (client new-tweet) ((user string) (status-text string))
  (declare (connected client (new-tweet string string)))
  (format T "~&Got new tweet from ~a: ~s" user status-text)
  (q+:add-item timeline (format NIL "@~a: ~a" user status-text)))

(defun process-message (message client)
  (format T "~&Message: ~a" message)
  (when (typep message 'chirp:status)
    (signal! client (new-tweet string string)
             (chirp:screen-name (chirp:user message))
             (chirp:xml-decode (chirp:text-with-expanded-urls message)))))

(defun main ()
  (format T "~&Starting Titter...")
  (if *logged-in*
      (format T "~&Already logged in, skipping...")
      (with-main-window (w (make-instance 'login))))
  (when *logged-in*
    (format T "~&Launching client...")
    (let ((thread))
      (with-main-window (w (make-instance 'client))
        (setf thread
              (bt:make-thread
               #'(lambda ()
                   (chirp:start-stream
                    :user #'(lambda (message)
                              (when thread
                                (process-message message w) T)))
                   (format T "~&Shutting down tweet stream"))
               :initial-bindings  `((*standard-output* . ,*standard-output*)))))
      (setf thread NIL))))
