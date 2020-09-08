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
    (unwind-protect (mapc (lambda (a)
                            (bt:destroy-thread a))
                          (list con-receive-thread))
      ;;this is just a shit solution for now, I will use a condition notify to tell them to quit
      (usocket:socket-close (listening-socket server)))))

(defparameter *errors* ())

(defun cleanup-connections (server)
  (with-accessors ((connections connections))
      server
    (when (connections server)
      (let* ((to-shutdown (remove-if #'null connections :key #'deletep))
             (to-keep (when to-shutdown (remove-if-not #'null connections :key #'deletep))))
        (mapc (lambda (con);;close all of the connections
                ;;might have to catch an error here not sure hence the lambda
                (when (grab-incoming-connection con t);;make sure we block, this *has* to be done
                  (usocket:socket-close (connection con))
                  (release-incoming-connection con)));;should probably swap this to an actual lock
              to-shutdown)
        (when to-keep;;no point doing setf if nothing was yeeted
          (setf connections to-keep))))))

(defun service-connections (server)
  (loop :do (mapc (lambda (con)
                    (when con
                      (when (grab-incoming-connection con)
                        (unwind-protect
                             (handler-case
                                 (if (timed-out-p con)
                                     ;;need to check for a timeout,
                                     ;;if it has then send a 'connection: close'
                                     (shutdown-an-incoming-connection server con)
                                     (service-incoming-connection server con))
                               (dirty-disconnect (c)
                                 (log:warn "Dirty disconnection by client: ~A" c)
                                 (setf (deletep con) t));;mark the connection to be removed
                               (graceful-disconnect ();;catch a client saying 'connection: close'
                                 ;;or a timeout telling us its been handled properly and now should
                                 ;;be removed
                                 (setf (deletep con) t)));;meaning the client connection can be removed
                          (release-incoming-connection con));;double unlock is fine
                        (release-incoming-connection con))))
                  (connections server))
            (cleanup-connections server);;remove dead connections
            (sleep 1)))

(defun make-server (&key (port 8080) (interface "0.0.0.0"))
  (let ((server (make-instance 'server
                               :port port :interface interface
                               :listening-socket (usocket:socket-listen interface port
                                                                        :reuse-address t
                                                                        :reuseaddress t))))
    (setf (con-receive-thread server)
          (bt:make-thread (lambda () (get-connections server))))
    (setf (serving-thread server)
          (bt:make-thread
           (lambda () (service-connections server))))
    server))

(defmethod get-connections ((server server))
  (with-accessors ((socket listening-socket)
                   (connections connections))
      server
    (loop :for con := (usocket:socket-accept socket :element-type '(unsigned-byte 8))
          :if (not (find con connections :test #'eq))
            :do (push (make-instance 'incoming-connection :connection con) connections)
          :else :do (sleep 1))))

(defmethod (setf connections) :before (new-val (server server))
  (log:info "Adding new connection: ~A to server: ~A" new-val server))

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
            (serve server con-stream))))
    ((or end-of-file stream-error) ()
      (signal-dirty-disconnect (con-stream incoming-connection)))))
    ;;    (condition (c);;this'll do for now
    ;;    (push c *errors*)
  ;;  (trivial-backtrace:print-backtrace c :output *error-output*))))

(defmethod shutdown-an-incoming-connection ((server server) incoming-connection)
  "Given an INCOMING-CONNECTION object this will send a HTTP response object telling the client
to shutdown ie containing the header 'Connection: close'"
  (let* ((handler (timeout-handler))
         (response (make-http-response server handler :504 :close t))
         (*standard-output* (body response)))
    (funcall (response-body-func handler))
    (send-response (con-stream incoming-connection) response)
    (signal-graceful-disconnect)));;we finish by telling the processor to delete the connection

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
