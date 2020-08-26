;;;; jupiter.lisp

(in-package #:jupiter)


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

(defmethod stop-server ((server server))
  (with-accessors ((serving-thread serving-thread)
                   (con-receive-thread con-receive-thread))
      server
    (mapcar #'bt:destroy-thread (list con-receive-thread serving-thread))
    ;;this is just a shit solution for now, I will use a condition notify to tell them to quit
    (usocket:socket-close (listening-socket server))))

(defmethod get-connections ((server server))
  (with-accessors ((socket listening-socket))
      server
    (loop :for con := (usocket:socket-accept socket :element-type '(unsigned-byte 8))
          :do (let ((incoming-con (make-instance 'incoming-connection :connection con)))
                (push incoming-con (connections server))))))

(defun service-incoming-connection (server incoming-connection)
  (when (grab-incoming-connection incoming-connection)
    (unwind-protect
         (handler-case
             (progn
               (unless (slot-boundp incoming-connection '%con-stream)
                 (setf (con-stream incoming-connection) (usocket:socket-stream
                                                         (connection incoming-connection))))
               (with-accessors ((con-stream con-stream))
                   incoming-connection
                 (with-open-stream (stream con-stream)
                   (serve server stream))))
           ((or end-of-file stream-error) ()
             (setf (connections server)
                   (remove incoming-connection (connections server) :test #'eq)))
           (condition (c);;this'll do for now
             (trivial-backtrace:print-backtrace c :output *error-output*)))
      (release-incoming-connection incoming-connection))))



;;;need to implement persistent connection using 'pipelines'
;;;connections are marked as closed by Connection: close
;;;if a connection times out then a Connection: close should be sent ie graceful shutdown
;;;all requests from a connection must be answered in order, this will happen naturally if you just
;;;read from the stream until there is nothing left to read.
;;;






(defmethod serve ((server server) stream)
  "Given an instance of SERVER and a STREAM this function will attempt to parse a HTTP request from
the STREAM and then call the associated handler."
  (let ((packet (parse-request stream)))
    (with-accessors ((http-method http-method)
                     (path path))
        packet
      ;;need to sort out something for 404-handler
      (handler-case 
          (let* ((handler (get-handler server http-method path));;grab associated handler object
                 (response (make-http-response server handler :200));;create response object
                 ;;using the handler and the server to fill in headers
                 (*standard-output* (body response)));;swap *standard-output* to the responses
            ;;stream in order to write directly to that stream
            (funcall (response-body-func handler) packet response)
            ;;call the handlers function where *standard-output* should be (body response)
            (send-response stream response));;send the collected down connection stream
        (no-associated-handler ()
          (let* ((handler (404-handler http-method path))
                 (response (make-http-response server handler :404))
                 (*standard-output* (body response)))
            (funcall (response-body-func handler))
            (send-response stream response))))
      ;;need to figure out how I will let the user send a vector down the stream
      ;;so they can send images etc
      )))
