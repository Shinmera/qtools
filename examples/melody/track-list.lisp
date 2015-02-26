#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.melody)
(named-readtables:in-readtable :qtools)

(define-widget track (QWidget)
  ((file :initarg :file :initform (error "FILE required."))))

(defmethod print-object ((track track) stream)
  (print-unreadable-object (track stream :type T :identity T)
    (format stream "~s" (enough-namestring (slot-value track 'file)))))

(define-subwidget (track source) (q+:make-phonon-mediasource
                                  (uiop:native-namestring file)))

(define-subwidget (track object) (q+:make-phonon-mediaobject track)
  (connect! object (meta-data-changed) track (info))
  (setf (q+:current-source object) source))

(define-subwidget (track playing) (q+:make-qpushbutton track)
  (setf (q+:fixed-size playing) (values 20 20))
  (setf (q+:flat playing) T)
  (setf (q+:contents-margins playing) (values 0 0 0 0)))

(define-subwidget (track no) (q+:make-qlabel "" track)
  (setf (q+:fixed-width no) 50))

(define-subwidget (track title) (q+:make-qlabel (pathname-name file) track)
  (setf (q+:fixed-width title) 400))

(define-subwidget (track artist) (q+:make-qlabel "" track)
  (setf (q+:fixed-width artist) 200))

(define-subwidget (track album) (q+:make-qlabel "" track)
  (setf (q+:fixed-width album) 200))

(define-subwidget (track layout) (q+:make-qhboxlayout track)
  (q+:add-widget layout playing)
  (q+:add-widget layout no)
  (q+:add-widget layout title)
  (q+:add-widget layout artist)
  (q+:add-widget layout album)
  (setf (q+:spacing layout) 0))

(define-slot (track info) ()
  ;; Apparently on some systems, such as mine, this slot
  ;; never gets called because metaDataChanged() never gets
  ;; signalled on the MediaObject. Don't ask me why, I
  ;; searched long and wide without finding anything about it.
  ;; sorry.
  (flet ((set-meta (label meta)
           (let ((data (first (q+:meta-data object meta))))
             (format T "~&~a Data ~a: ~a" track meta data)
             (setf (q+:text label) (or data "?")))))
    (set-meta no (q+:phonon.tracknumber-meta-data))
    (set-meta title (q+:phonon.title-meta-data))
    (set-meta artist (q+:phonon.artist-meta-data))
    (set-meta album (q+:phonon.album-meta-data))))

(defun (cl:setf playing) (bool track)
  (with-slots-bound (track track)
    (if bool
        (setf (q+:icon playing) (q+:standard-icon (q+:style track) (q+:qstyle.sp-media-play)))
        (setf (q+:icon playing) (q+:make-qicon)))))

(defun playing (track)
  (with-slots-bound (track track)
    (not (q+:is-null (q+:icon playing)))))

(defun source (track)
  (slot-value track 'source))

(defun (cl:setf source) (new-source track)
  (with-slots-bound (track track)
    (finalize source)
    (setf source new-source)
    (setf (q+:current-source object) new-source)))

(defun file (track)
  (slot-value track 'file))

(defun (cl:setf file) (pathname track)
  (with-slots-bound (track track)
    (setf file pathname)
    (setf (source track) (q+:make-phonon-mediasource
                          (uiop:native-namestring track)))))

(defun info (track)
  (with-slots-bound (track track)
    (format NIL "~a~@[ — ~a~]~@[ — ~a~]"
            (q+:text title)
            (unless (string= (q+:text album) "") (q+:text album))
            (unless (string= (q+:text artist) "") (q+:text artist)))))

;; We have to do this kind of idiotic garbage
;; because of Qt's impossibly studid handling
;; of table cell widgets.
;; To be more precise, we need to have a container
;; as we cannot use setCellWidget directly to
;; move rows in our dropEvent (which we have to
;; override to fix Qt's broken default behaviour)
;; as setCellWidget /deletes/ the widget that's
;; at the position you're setting to.
;; Instead we have to set the contained item
;; as we can do that without causing any weird
;; deletion garbage.
;;
;; Needless to say I wish such annoying kludges
;; weren't necessary in example code like this,
;; but my excuse is that I'm showing off Qtools
;; and not necessarily Qt, so I don't really
;; care if I rant about it.
(define-widget track-list-container (QWidget)
  ((inner :initarg :inner :initform (error "INNER required."))))

(define-subwidget (track-list-container layout) (q+:make-qhboxlayout track-list-container)
  (q+:add-widget layout inner)
  (setf (q+:spacing layout) 0)
  (setf (q+:margin layout) 0))

(defun inner (container)
  (slot-value container 'inner))

(defun (cl:setf inner) (widget container)
  (with-slots-bound (container track-list-container)
    (q+:remove-widget layout inner)
    (q+:add-widget layout widget)
    (setf inner widget)))

(define-widget track-list (QTableWidget)
  ())

(define-initializer (track-list setup)
  (setf (q+:drag-enabled track-list) T)
  (setf (q+:accept-drops track-list) T)
  (setf (q+:accept-drops (q+:viewport track-list)) T)
  (setf (q+:drag-drop-overwrite-mode track-list) NIL)
  (setf (q+:drop-indicator-shown track-list) T)
  (setf (q+:drag-drop-mode track-list) (q+:qabstractitemview.internal-move))
  (setf (q+:selection-mode track-list) (q+:qabstractitemview.single-selection))
  (setf (q+:selection-behavior track-list) (q+:qabstractitemview.select-rows))
  (setf (q+:stretch-last-section (q+:horizontal-header track-list)) T)
  (setf (q+:column-count track-list) 1)
  (q+:hide (q+:horizontal-header track-list))
  (q+:hide (q+:vertical-header track-list)))

(defun drop-row (list event)
  (let ((index (q+:index-at list (q+:pos event))))
    (min
     (or
      (when (and (q+:contains (q+:rect (q+:viewport list)) (q+:pos event))
                 (q+:is-valid index))
        (let ((rect (q+:visual-rect list index))
              (pos (q+:pos event)))
          (when (q+:contains rect pos)
            (if (< (- (q+:bottom rect) (q+:y pos)) 2)
                (1+ (q+:row index))
                (q+:row index)))))
      0)
     (1- (q+:row-count list)))))

(define-override (track-list drop-event) (ev)
  ;; We get to do some manual calculation of drop rows
  ;; because apparently the default behaviour of
  ;; QTableWidget on row dropping is screwed.
  ;; It replaces rows instead of moving them. Great.
  (when (and (eql (q+:source ev) track-list)
             (not (q+:is-accepted ev)))
    (when (q+:contains (q+:rect (q+:viewport track-list)) (q+:pos ev))
      (let* ((row (q+:current-row track-list))
             (current (track track-list row))
             (dropping (drop-row track-list ev)))
        (if (< row dropping)
            (loop for i from row below dropping
                  do (setf (inner (q+:cell-widget track-list i 0))
                           (inner (q+:cell-widget track-list (1+ i) 0))))
            (loop for i downfrom row above dropping
                  do (setf (inner (q+:cell-widget track-list i 0))
                           (inner (q+:cell-widget track-list (1- i) 0)))))
        (setf (inner (q+:cell-widget track-list dropping 0)) current)))))

(defun add-track (track-list track)
  (let ((track (etypecase track
                 (string (make-instance 'track :file (uiop:parse-native-namestring track)))
                 (pathname (make-instance 'track :file track))
                 (track track)))
        (index (q+:row-count track-list)))
    (format T "~&Adding track ~a at ~d" track index)
    (q+:insert-row track-list index)
    (setf (q+:cell-widget track-list index 0)
          (make-instance 'track-list-container :inner track))
    (q+:resize-rows-to-contents track-list)
    track))

(defun find-track (track-list thing)
  (loop for i from 0 below (q+:row-count track-list)
        for track = (track track-list i)
        do (when (or (and (typep thing 'track)
                          (eql thing track))
                     (and (typep thing 'pathname)
                          (uiop:pathname-equal thing (file track)))
                     (and (qtypep thing "Phonon::MediaSource")
                          (q+:= thing (source track))))
             (return track))))

(defun select-track (track-list select)
  (loop for i from 0 below (q+:row-count track-list)
        for track = (track track-list i)
        do (when (or (and (typep select 'track)
                          (eql select track))
                     (and (typep select 'pathname)
                          (uiop:pathname-equal select (file track)))
                     (and (qtypep select "Phonon::MediaSource")
                          (q+:= select (source track))))
             (q+:select-row track-list i)
             (return track))))

(defun current-track (track-list)
  (loop for i from 0 below (q+:row-count track-list)
        for track = (track track-list i)
        do (when (playing track)
             (return-from current-track (values track i))))
  (values NIL -1))

(defun next-track (track-list)
  (let ((current (nth-value 1 (current-track track-list))))
    (when (< current (q+:row-count track-list))
      (track track-list (1+ current)))))

(defun track (track-list n)
  (when (< -1 n (q+:row-count track-list))
    (inner (q+:cell-widget track-list n 0))))

(defun tracks (track-list)
  (loop for i from 0 below (q+:row-count track-list)
        collect (inner (q+:cell-widget track-list i 0))))
