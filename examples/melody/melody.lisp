#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.melody)
(named-readtables:in-readtable :qtools)

(defvar *trackinfo* '(("#" "TRACKNUMBER" 50) ("Title" "TITLE") ("Album" "ALBUM") ("Artist" "ARTIST")))

;;;;;
;; Widget layout
(define-widget player (QMainWindow)
  ())

(define-subwidget (player audio-output) (q+:make-phonon-audiooutput (q+:phonon.music-category) player))

(define-subwidget (player media-object) (q+:make-phonon-mediaobject player)
  (q+:set-tick-interval media-object 1000)
  (q+:phonon-create-path media-object audio-output))

(define-subwidget (player list) (make-instance 'track-list))

(define-subwidget (player info) (q+:make-qlabel player))

(define-subwidget (player open) (q+:make-qpushbutton "Open" player))

(define-subwidget (player time) (q+:make-qlabel "0:00" player))

(define-subwidget (player length) (q+:make-qlabel "0:00" player))

(define-subwidget (player slider) (q+:make-phonon-seekslider player)
  (setf (q+:media-object slider) media-object)
  (setf (q+:icon-visible slider) NIL))

(define-subwidget (player volume) (q+:make-phonon-volumeslider player)
  (setf (q+:audio-output volume) audio-output)
  (setf (q+:size-policy volume) (values (q+:qsizepolicy.maximum)
                                        (q+:qsizepolicy.maximum))))

(defun make-button (widget icon text)
  (let ((button (q+:make-qpushbutton widget)))
    (setf (q+:icon button) (q+:standard-icon (q+:style widget) icon))
    (setf (q+:tool-tip button) text)
    (setf (q+:flat button) T)
    button))

(define-subwidget (player play) (make-button player (q+:qstyle.sp-media-play) "Play"))

(define-subwidget (player pause) (make-button player (q+:qstyle.sp-media-pause) "Pause")
  (q+:hide pause))

(define-subwidget (player stop) (make-button player (q+:qstyle.sp-media-stop) "Stop"))

(define-subwidget (player layout) (q+:make-qvboxlayout)
  (setf (q+:window-title player) "Melody")
  (q+:add-widget layout list)
  (q+:add-widget layout info)
  (let ((inner (q+:make-qhboxlayout)))
    (q+:add-widget inner open)
    (q+:add-widget inner play)
    (q+:add-widget inner pause)
    (q+:add-widget inner stop)
    ;;(q+:add-widget inner time)
    (q+:add-widget inner slider)
    ;;(q+:add-widget inner length)
    (q+:add-widget inner volume)
    (q+:add-layout layout inner))
  (let ((widget (q+:make-qwidget player)))
    (setf (q+:layout widget) layout)
    (setf (q+:central-widget player) widget)))

(define-menu (player File)
  (:item ("Open" (ctrl o))
         (open-file player))
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close player)))

(define-menu (player Help)
  (:item "About"
         (q+:qmessagebox-information player "About"
                                     "This is a small example to illustrate writing a more complex Qt application using CommonQt and Qtools.")))

;;;;;
;; Slots
(define-slot (player play play) ()
  (declare (connected play (released)))
  (format T "~&Playing.")
  (q+:play media-object))

(define-slot (player pause pause) ()
  (declare (connected pause (released)))
  (format T "~&Pausing.")
  (q+:pause media-object))

(define-slot (player stop stop) ()
  (declare (connected stop (released)))
  (format T "~&Stopping.")
  (q+:stop media-object))

(define-slot (player open open-file) ()
  (declare (connected open (released)))
  (let ((files (q+:qfiledialog-get-open-file-names player "Select Files"
                                                   (q+:qdesktopservices-storage-location (q+:qdesktopservices.music-location)))))
    (dolist (file files)
      (add-track list file))))

(define-slot (player track-selected) ((row int) (col int))
  (declare (connected list (cell-double-clicked int int)))
  (declare (ignore col))
  (let ((track (track list row)))
    (setf (q+:current-source media-object)
          (source track))
    (play player)))

(defun reset-playing (list)
  (dolist (track (tracks list))
    (setf (playing track) NIL)))

(define-slot (player media-changed) ((new-state "Phonon::State" T) (old-state "Phonon::State" T))
  (declare (connected media-object (state-changed "Phonon::State" "Phonon::State")))
  (declare (ignore old-state))
  (cond ((enum-equal new-state (q+:phonon.error-state))
         (q+:qmessagebox-warning player "Error" (q+:error-string media-object)))
        ((enum-equal new-state (q+:phonon.playing-state))
         (setf (q+:enabled stop) T)
         (q+:show pause)
         (q+:hide play))
        ((enum-equal new-state (q+:phonon.paused-state))
         (setf (q+:enabled stop) T)
         (q+:show play)
         (q+:hide pause))
        ((enum-equal new-state (q+:phonon.stopped-state))
         (setf (q+:enabled stop) NIL)
         (q+:show play)
         (q+:hide pause))
        ((enum-equal new-state (q+:phonon.buffering-state)))
        (T (error "WTF? Enumerated all possible Phonon::States. Unknown state ~s" new-state))))

(define-slot (player source-changed) ((source "Phonon::MediaSource" T))
  (declare (connected media-object (current-source-changed "Phonon::MediaSource")))
  (reset-playing list)
  (let ((track (find-track list source)))
    (setf (playing track) T)
    (setf (q+:text info) (info track))))

;;;; This doesn't seem to work so far, for some reason
;;;; trying to reference the qint64/long-long value gives
;;;; me unhandled memory faults.
;;
;; (defun qint64 (val)
;;   (etypecase val
;;     (integer val)
;;     (cffi:foreign-pointer (cffi:mem-ref val :long-long))))
;;
;; (defun parse-time (miliseconds)
;;   (let* ((secs (/ (print miliseconds) 1000))
;;          (hours (floor (/ secs (* 60 60))))
;;          (mins (floor (/ secs 60)))
;;          (secs (mod secs 60)))
;;     (format NIL "~:[~d~;~*~]~d:~d" (= 0 hours) hours mins secs)))
;;
;; (define-slot (player tick) ((current-time "qint64" T))
;;   (declare (connected media-object (tick "qint64")))
;;   (setf (q+:text time) (parse-time (qint64 current-time))))
;;
;; (define-slot (player length-changed) ((total-time "qint64" T))
;;   (declare (connected media-object (total-time-changed "qint64")))
;;   (setf (q+:text length) (parse-time (qint64 total-time))))

(define-slot (player about-to-finish) ()
  (declare (connected media-object (about-to-finish)))
  (let ((next-track (print (next-track list))))
    (when next-track
      (q+:enqueue media-object (source next-track)))))

(defun main ()
  (with-main-window (window (make-instance 'player))))

