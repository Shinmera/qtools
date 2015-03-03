#|
 This file is a part of Qtools
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools.game)

(defclass world ()
  ((chunks :initform () :accessor chunks)))

(defun add-chunk (chunk world)
  (push chunk (chunks world)))

(defun remove-chunk (chunk world)
  (setf (chunks world) (delete chunk (chunks world))))

(defun chunk-at (world x y)
  (dolist (chunk (chunks world))
    (when (visible chunk (point x y))
      (return chunk))))

(defmethod paint ((world world) painter)
  (dolist (chunk (chunks world))
    (paint chunk painter)))

(defun load-world (file &optional (world (make-instance 'world)))
  (with-open-file (stream file :direction :input)
    (let ((data (read stream)))
      (setf (chunks world) (mapcar #'eval (getf data :world)))
      (remf data :world)
      (values world data))))

(defun save-world (file world &key if-exists)
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (let ((*print-readably* T))
      (print (list :world (chunks world)
                   :file file
                   :time (get-universal-time))
             stream))
    file))

(defgeneric update (thing world)
  (:method (thing (world world))
    thing)
  (:method ((all (eql T)) (world world))
    (dolist (chunk (chunks world))
      (update chunk world))))
