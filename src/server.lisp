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

(defmethod stop-server :before ((server server))
  (log:info "Stopping server: ~A" server))

(defmethod stop-server ((server server))
  (with-accessors ((serving-thread serving-thread)
                   (con-receive-thread con-receive-thread))
      server
    (let ((done nil))
      (unwind-protect
           (progn 
             (usocket:socket-close (listening-socket server))
             (setf done t))
        (unless done 
          (usocket:socket-close (listening-socket server)))))
    (stop-threads server)))

(defmethod stop-server :after ((server server))
  (log:info "Successfully shutdown server"))

(defparameter *errors* ())

(defmacro grab-connection (connection block &body body)
  (alexandria:with-gensyms (unlocked)
    `(let ((,unlocked nil))
       (when (grab-incoming-connection ,connection ,block)
         (unwind-protect (locally ,@body)
           (unless ,unlocked
             (release-incoming-connection ,connection)))
         (unless ,unlocked
           (release-incoming-connection ,connection))))))

(defmethod push-queue (queue (con incoming-connection))
  (queues:qpush queue con))
(defmethod push-queue (queue con)
  nil)

(defun service-connections (server)
  (with-accessors ((connections connections)
                   (serving-thread serving-thread))
      server
    (do ((con (queues:qpop connections) (queues:qpop connections)));;returns nil if empty
        ((shutdownp serving-thread) t);;check if the thread has been told to stop
      (when con
        (grab-connection con nil;;dont block
          (handler-case
              (if (timed-out-p con)
                  ;;need to check for a timeout,
                  ;;if it has then send a 'connection: close'
                  (progn
                    (log:info "Connection timed out ~A" con)
                    (shutdown-an-incoming-connection server con));;this will close the socket as well
                  (progn (service-incoming-connection server con)
                         (push-queue connections con)));;re-add con to the queue
            (dirty-disconnect (c)
              (log:warn "Dirty disconnection by client: ~A" c)) ;;don't re-add the con to the queue
            (graceful-disconnect ()
              (usocket:socket-close (connection con)))
            (condition (c);;this'll do for now
              (push c *errors*)
              (log:error "Error: ~A" c)
              (trivial-backtrace:print-backtrace c :output *error-output*)))));;catch a client saying 'connection: close'
      ;;or a timeout telling us its been handled properly and now should be removed
      ;;meaning the client connection can be removed.
      (sleep 0.0001))))

(defmethod stop-threads :before ((server server))
  (log:info "Stopping processing threads"))

(defmethod stop-threads ((server server))
  "Sets shutdownp within the servers thread-controllers to t, meaning the threads should see this
and finish."
  (setf (shutdownp (con-receive-thread server))
        t)
  (setf (shutdownp (serving-thread server))
        t))

(defmethod stop-threads :after ((server server))
  (with-accessors ((con-receive-thread con-receive-thread)
                   (serving-thread serving-thread))
      server
    (loop :if (and (not (bt:thread-alive-p (thread con-receive-thread)))
                   (not (bt:thread-alive-p (thread serving-thread))))
            :do (log:info "Processing threads safely shutdown")
                (return t)
          :else :do (sleep 0.001))))

(defun make-server (&key (port 8080) (interface "0.0.0.0"))
  (let ((server (make-instance 'server
                               :port port :interface interface
                               :listening-socket (usocket:socket-listen interface port
                                                                        :reuse-address t
                                                                        :reuseaddress t))))
    (setf (thread (con-receive-thread server))
          (bt:make-thread (lambda () (get-connections server))))
    (setf (thread (serving-thread server))
          (bt:make-thread
           (lambda () (service-connections server))))
    server))

(defmethod get-connections ((server server))
  (with-accessors ((socket listening-socket)
                   (connections connections)
                   (con-receive-thread con-receive-thread))
      server
    (do ((con (timeout-body (1 nil)
                (usocket:socket-accept socket :element-type '(unsigned-byte 8)))
              (timeout-body (1 nil)
                (usocket:socket-accept socket :element-type '(unsigned-byte 8)))))
        ((shutdownp con-receive-thread) t);;need to come up with a nice way to stop threads
      (when con
        (log:info "Adding new connection: ~A to server: ~A" con server)
        (queues:qpush connections (make-instance 'incoming-connection :connection con)))
      (sleep 0.0001))))

(defmethod service-incoming-connection :before ((server server) con)
  (when (log:debug)
    (log:debug "Servicing connection: ~S in server: ~A" con server)))

(defmethod service-incoming-connection ((server server)(incoming-connection incoming-connection))
  (handler-case
      (progn
        (with-accessors ((connection connection)
                         (con-stream con-stream))
            incoming-connection
          (unless (and con-stream connection)
            (setf con-stream (usocket:socket-stream connection)))
          ;;(print con-stream)
          (when (and con-stream (listen con-stream));;make sure there is actually something to read
            (unless (open-stream-p con-stream)
              (signal-dirty-disconnect con-stream))
            (reset-last-used incoming-connection);;gotta make sure that the timeout is reset
            (serve server con-stream))))
    ((or end-of-file stream-error) ()
      (signal-dirty-disconnect (con-stream incoming-connection)))))

(defmethod shutdown-an-incoming-connection ((server server) incoming-connection)
  "Given an INCOMING-CONNECTION object this will send a HTTP response object telling the client
to shutdown ie containing the header 'Connection: close'"
  (let* ((handler (timeout-handler))
         (response (make-http-response server handler :504 :close t))
         (*standard-output* (body response)))
    (funcall (response-body-func handler))
    (send-response (con-stream incoming-connection) response)
    (usocket:socket-close (connection incoming-connection))
    (signal-graceful-disconnect)));;we finish by telling the processor to delete the connection

(defmethod serve ((server server) stream)
  "Given an instance of SERVER and a STREAM this function will attempt to parse a HTTP request from
the STREAM and then call the associated handler."
  (let ((packet (parse-request stream)))
    (with-accessors ((http-method http-method)
                     (path path))
        packet
      (handler-case 
          (let* ((handler (get-handler server http-method path));;grab associated handler object
                 (response (make-http-response server handler :200));;create response object
                 ;;using the handler and the server to fill in headers
                 (*standard-output* (body response)));;swap *standard-output* to the responses
            ;;stream in order to write directly to that stream
            (log:info "Responding to method: ~A at path: ~A" http-method path)
            (funcall (response-body-func handler) packet response)
            ;;call the handlers function where *standard-output* should be (body response)
            (send-response stream response);;send the collected down connection stream
            (when (wants-to-close-p packet);;tell the connection processor that we should shutdown
              (signal-graceful-disconnect)));;the connection
        (no-associated-handler (c)
          (let* ((handler (404-handler http-method path))
                 (response (make-http-response server handler :404 :close t))
                 (*standard-output* (body response)))
            (log:warn "No handler associated with the URL: ~A" (n-a-h-url c))
            (funcall (response-body-func handler))
            (send-response stream response)
            (when (wants-to-close-p packet)
              (signal-graceful-disconnect))))))))
