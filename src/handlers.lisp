(in-package :jupiter)


(defparameter *valid-methods*
  (list :POST :GET :DELETE :HEAD :PUT :CONNECT :TRACE :OPTIONS :PATCH))


(defclass handler ()
  ((%creation-time
    :initarg :creation-time
    :accessor creation-time
    :type string)
   (%last-modified
    :initarg :last-modified
    :accessor last-modified
    :type string)
   (%response-body-func
    :initarg :response-body-func
    :type function
    :accessor response-body-func)))

(define-condition no-associated-handler ()
  ((n-a-h-url
    :initarg :n-a-h-url
    :accessor n-a-h-url)
   (n-a-h-http-method
    :initarg :n-a-h-http-method
    :accessor n-a-h-http-method)))

(defmethod print-object ((obj no-associated-handler) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "Failed to find a handler for URL: ~S and METHOD: ~S~%"
            (n-a-h-url obj)
            (n-a-h-http-method obj))))

(defun make-handlers-hash ()
  (let ((hash-table (make-hash-table :test #'eq)))
    (mapcar (lambda (method)
              (setf (gethash method hash-table)
                    (make-hash-table :test #'equalp)))
            *valid-methods*)
    hash-table))

(defmethod timed-out-p ((con incoming-connection))
  (let ((now (get-universal-time)))
    (<= (timeout con) (- now (last-used con)))))

(defmethod reset-last-used ((con incoming-connection))
  (setf (last-used con) (get-universal-time)))

(defun valid-method-p (key)
  (check-type key keyword)
  (member key *valid-methods*))

(deftype http-method () `(satisfies valid-method-p))

(defun get-handler (server method url)
  (check-type method http-method)
  (let ((fun (gethash url (gethash method (handlers server)))))
    (if fun
        fun
        (error 'no-associated-handler :n-a-h-url url :n-a-h-http-method method))))

(defun set-handler (server method url handler)
  (check-type handler handler)
  (check-type method http-method)
  (setf (gethash url (gethash method (handlers server))) handler))

(defmacro define-handler (server (method path) lambda)
  (check-type method http-method)
  (check-type path string)
  (alexandria:with-gensyms (handler time)
    `(handler-case
         (let ((,handler (get-handler ,server ,method ,path)))
           (with-accessors ((last-modified last-modified)
                            (response-body-func response-body-func))
               ,handler
             (setf last-modified (time-now))
             (setf response-body-func ,lambda)))
       (no-associated-handler ()
         (let* ((,time (time-now))
                (,handler (make-instance 'handler :response-body-func ,lambda
                                                  :creation-time ,time
                                                  :last-modified ,time)))
           (set-handler ,server ,method ,path ,handler))))))


(defmethod serve-static-handler ((server server) (handler handler) http-status stream close)
  (let* ((response (make-http-response server handler :403 :close close))
         (*standard-output* (body response)))
    (funcall (response-body-func handler) nil response)
    (handler-case
        (send-response stream response)
      (dirty-disconnect (c)
        (log:warn "Dirty disconnection by client: ~A" c)))))

(defun 404-handler (method url)
  (make-instance 'handler :response-body-func 
                 (lambda () (format t "404 not found~%URL: ~S~%METHOD: ~S~%" method url))
                 :creation-time (time-now)
                 :last-modified (time-now)))

(defun timeout-handler ()
  (make-instance 'handler
                 :response-body-func
                 (lambda () (format t "504 connection timed out"))
                 :creation-time (time-now)
                 :last-modified (time-now)))

(defun 400-handler ()
  (make-instance 'handler
                 :response-body-func
                 (lambda () (format t "400 Bad Request"))
                 :creation-time (time-now)
                 :last-modified (time-now)))

(defun 413-handler ()
  (make-instance 'handler
                 :response-body-func
                 (lambda () (format t "413 Payload Too Large"))
                 :creation-time (time-now)
                 :last-modified (time-now)))






