(in-package :jupiter)
;;;; this file contains the contains the implementation of thread controllers, serving threads,
;;;;and threadpools

#|notes
Need to put a hardcap on the number of threads otherwise a large number of connections will effectively
DOS the machine.
Could have a few threads always going, like a master thread or something.



|#
(defclass thread-controller ()
  ((thread
    :accessor thread
    :type bt:thread)
   (shutdownp
    :accessor shutdownp
    :type boolean
    :initform nil))
  (:metaclass metalock:metalock))

(defclass serving-thread (thread-controller)
  ((%connection-queue
    :accessor connection-queue
    :initform (queues:make-queue :simple-cqueue :minimum-size 1))
   (%max-count
    :accessor max-count
    :initform 3);;this seems to be the optimal, however I don't think the queue
   ;;is the most optimal way of doing this
   (%current-count;;make sure when we don't push the con again that we decrement this!!
    :accessor current-count
    :initform 0))
  (:metaclass metalock:metalock));;10 maximum conns at once

(defclass thread-pool ()
  ((%threads
    :accessor threads
    :type list
    :initform ())
   (%total-connections
    :accessor total-connections
    :type integer
    :initform 0)))

(define-condition no-empty-serving-threads ()
  ())

(defun make-serving-thread (server connection)
  (let ((st (make-instance 'serving-thread)))
    (push-queue (connection-queue st) connection)
    (incf (current-count st))
    (setf (thread st) (bt:make-thread (lambda () (service-connection-pool server st))))
    st))

(defmethod fullp ((serving-thread serving-thread))
  (with-accessors ((c-c current-count)
                   (m-c max-count))
      serving-thread
    (= c-c m-c)))

(defmethod all-fullp ((thread-pool thread-pool))
  (every #'fullp (threads thread-pool)))

(defmethod emptyp ((serving-thread serving-thread))
  (zerop (current-count serving-thread)))

(defmethod get-first-with-free-space ((thread-pool thread-pool))
  "Returns the first serving-thread within THREAD-POOL that has free space, if none do then signals 
the condition 'no-empty-serving-threads."
  (loop :for s-t :in (threads thread-pool)
        :if (not (fullp s-t))
          :do (return s-t)
        :finally (error 'no-empty-serving-threads)))

(defmethod create-and-add-new-serving-thread ((server server) connection)
  (with-accessors ((pool con-serve-pool))
      server
    (let ((serving-thread (make-serving-thread server connection)))
      (push serving-thread (threads pool))
      (incf (total-connections pool)))))

(defmethod add-connection-to-serving-thread ((serving-thread serving-thread) (server server) con)
  (push-queue (connection-queue serving-thread) con)
  (incf (current-count serving-thread))
  (incf (total-connections (con-serve-pool server))))

(defmethod remove-empty-serving-threads ((thread-pool thread-pool))
  (setf (threads thread-pool) (remove-if #'emptyp (threads thread-pool))))

(defmethod decrease-connection-count ((server server)(serving-thread serving-thread))
  (decf (current-count serving-thread))
  (decf (total-connections (con-serve-pool server))))


