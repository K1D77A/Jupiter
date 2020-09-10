(in-package :jupiter)


(defparameter *valid-methods*
  (list :POST :GET :DELETE :HEAD :PUT :CONNECT :TRACE :OPTIONS :PATCH))

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



