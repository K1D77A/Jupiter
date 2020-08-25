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
  (usocket:socket-close (listening-socket server)))

(defmethod get-connections ((server server))
  (with-accessors ((socket listening-socket))
      server
    (loop :for con := (usocket:socket-accept socket)
          :do
             (handler-case
                 (with-open-stream (stream (usocket:socket-stream con))
                   (serve server stream))
               (condition (c);;this'll do for now
                 (trivial-backtrace:print-backtrace c :output *error-output*))))))

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
