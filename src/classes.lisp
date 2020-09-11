(in-package :jupiter)

(defclass http-packet ()
  ((%http-version
    :type string
    :accessor http-version)
   (%method
    :type keyword
    :accessor http-method)
   (%path
    :type string
    :accessor path)
   (%parameters
    :type list
    :accessor parameters)
   (%headers
    :type list
    :accessor headers)
   (%content-headers
    :type list
    :accessor content-headers)
   (%body
    :accessor body)))

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

(defclass http-response ()
  ((%http-version
    :type string
    :initarg :http-version
    :accessor http-version)
   (%status-code
    :type keyword
    :initarg :status-code
    :accessor status-code)
   (%headers
    :type list
    :initarg :headers
    :accessor headers)
   (%body
    :type (or stream array)
    :initform (make-string-output-stream)
    :accessor body))
  (:documentation "A class used to create the headers for a response that are sent to a requester"))


(defun make-http-response (server handler status-code &key (close nil))
  (let ((headers (list (list "Date" (time-now))
                       (list "Server" (server-version server))
                       (list "Last-Modified" (last-modified handler))
                       (list "Connection" (if close
                                              "close"
                                              "keep-alive"))
                       (list "Content-Type" "text/html"))))
    (make-instance 'http-response
                   :headers headers
                   :status-code status-code
                   :http-version (http-version server))))

(defun make-handlers-hash ()
  (let ((hash-table (make-hash-table :test #'eq)))
    (mapcar (lambda (method)
              (setf (gethash method hash-table)
                    (make-hash-table :test #'equalp)))
            *valid-methods*)
    hash-table))

(defclass incoming-connection ()
  ((%connection
    :accessor connection
    :initarg :connection)
   (%timeout
    :accessor timeout
    :initform 15
    :type integer)
   (%in-use
    :accessor in-use
    :initform nil
    :type boolean)
   (%con-stream;;currently not used not sure if keeping a stream around is a better idea or not 
    :accessor con-stream
    :initform nil
    :initarg :con-stream)
   (%last-used
    :accessor last-used
    :type integer
    :initform (get-universal-time)))
  (:metaclass metalock:metalock))

(defmethod timed-out-p ((con incoming-connection))
  (let ((now (get-universal-time)))
    (<= (timeout con) (- now (last-used con)))))

(defmethod reset-last-used ((con incoming-connection))
  (setf (last-used con) (get-universal-time)))

(defun grab-incoming-connection (incoming-connection &optional (block nil))
  "Returns either nil or the incoming-connection, if nil is returned it means that the 
incoming-connection is in use already."
  (if block
      (loop :for in-use := (in-use incoming-connection)
            :if in-use
              :do (sleep 0.00001);;wait this long then try again
            :else :do (setf (in-use incoming-connection) t)
                      (return t))
      (unless (in-use incoming-connection)
        (setf (in-use incoming-connection) t)
        incoming-connection)))

(defun release-incoming-connection (incoming-connection)
  (setf (in-use incoming-connection) nil)
  t)

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

;;;I want to have a pool of threads starting with 1. Each thread will have its *own* connection queue
;;;when the connection receiver gets a new connection it will pass one of these connections to a
;;;single handler, each thread should handle roughly 10 connections each I reckon
;;;and when they have less than 10 they will mark themselves as ready to receive more and how much
;;;room they have
;;;for the receiving thread I think it will randomly place them in available thread queues
;;;and will have to manage starting up new threads when all the queues are full.
;;;each thread will finish when its queue empties completely.

(defclass server ()
  ((%http-version
    :initform "HTTP/1.1"
    :accessor http-version)
   (%server-version
    :initform (format nil "Jupiter/~A (~A ~A)"
                      (asdf:component-version (asdf:find-system :jupiter))
                      (asdf/common-lisp:lisp-implementation-type)
                      (asdf/common-lisp:lisp-implementation-version))
    :accessor server-version)
   (%port
    :accessor port
    :initarg :port
    :type integer
    :initform 80)
   (%interface
    :accessor interface
    :initarg :interface
    :initform "0.0.0.0")
   (%handlers
    :accessor handlers
    :initform (make-handlers-hash))
   (%con-serve-pool
    :initform (make-instance 'thread-pool)
    :accessor con-serve-pool
    :documentation "This is a pool of threads that are used to serve data to connections")
   (%con-receive-thread
    :initform (make-instance 'thread-controller)
    :accessor con-receive-thread)
   (%listening-socket
    :accessor listening-socket
    :initarg :listening-socket)))

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


(defmethod print-object ((obj http-response) stream)
  (flet ((fun ()
           (response-format stream "~A ~A"
                            (http-version obj)
                            (code->status (status-code obj)))
           (mapc (lambda (lst)
                   (serialize-header stream (first lst) (second lst)))
                 (headers obj))
           (response-format stream "")));;important to add a CRLF before body
    (if *print-readably*
        (fun)
        (print-unreadable-object (obj stream)
          (fun)))))


(defclass cookie ()
  ((%cookie
    :initarg :cookie 
    :accessor cookie)
   (%crumb
    :initarg :crumb 
    :accessor crumb)
   (%domain
    :initarg :domain
    :accessor domain)
   (%path
    :initform "/"
    :initarg :path
    :accessor path)
   (%make-session-p
    :initform nil
    :initarg :make-session-p
    :accessor make-session-p
    :documentation "If this is true then expires is ignored and this cookie becomes a session cookie")
   (%expires
    :initarg :expires
    :initform nil
    :accessor expires)
   (%comment
    :initarg :comment
    :initform nil
    :accessor comment)
   (%encodep
    :initarg :encodep
    :initform nil
    :accessor encodep
    :documentation "When set to t this will mean that cooking and crumb are percent encoded.")
   (%http-only
    :initarg :http-only
    :initform t
    :accessor http-only)))



(defgeneric serialize-header (stream key val)
  (:documentation "Need a way to conditionally serialize parts of headers because sometimes the val
of a header isn't just a normal number but a complex object like a cookie, these have to be 
handled differently"))

(defmethod serialize-header (stream key (val cookie))  
  (format stream "~A:" key)
  (print-object val stream)
  (response-format stream "") ;;add the CRLF on the end
  )

(defmethod serialize-header (stream key val)
  (response-format stream "~A: ~A" key val))


;;;also need to have a Secure header at some point but no support for HTTPS yet

(defun make-cookie (key val &key (path "/") (make-session-p nil) (expires 7) (http-only t)
                              (domain nil) (comment nil) (encode nil)) 
  (make-instance 'cookie :http-only http-only
                         :expires (if expires
                                      (time+ expires :day)
                                      nil)
                         :encodep encode
                         :make-session-p make-session-p
                         :path path
                         :domain domain
                         :comment comment
                         :crumb val
                         :cookie key))

(defmethod print-object ((obj cookie) stream)
  ;;gotta convert cookie from an object to something like
  ;;lu=Rg3vHJZnehYLjVg7qi3bZjzg; Expires=Tue, 15 Jan 2013 21:47:38 GMT; Path=/; Domain=.example.com; ;;HttpOnly
  (flet ((fun ()
           (with-accessors ((cookie cookie)
                            (crumb crumb)
                            (path path)
                            (make-session-p make-session-p)
                            (expires expires)
                            (http-only http-only)
                            (comment comment)
                            (encodep encodep))
               obj
             (format stream "~A=~A; "
                     (if encodep
                         (percent-encoding:encode cookie)
                         cookie)
                     (if encodep
                         (percent-encoding:encode crumb)
                         crumb));;Cookie=Crumb;
             (when (and expires (not make-session-p))
               (format stream "Expires=~A; " expires))
             (when path
               (format stream "Path=~A; " path))
             (when comment
               (format stream "Comment=~A; " comment))
             (when http-only
               (format stream "HttpOnly")))))
    (if *print-readably*
        (fun)
        (print-unreadable-object (obj stream)
          (fun)))))



;;;condtions

(define-condition dirty-disconnect ()
  ((d-d-con
    :initarg :d-d-con
    :accessor d-d-con))
  (:documentation "Signalled when a client has closed a stream without sending a 'connection: close'"))

(defmethod print-object ((obj dirty-disconnect) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "Connection ended incorrectly~%Connection: ~A~%"
            (d-d-con obj))))

(defun signal-dirty-disconnect (connection)
  (error 'dirty-disconnect :d-d-con connection))

(define-condition graceful-disconnect ()
  ()
  (:documentation "Signalled when a client sends a connectdion: close' header"))

(defun signal-graceful-disconnect ()
  (error 'graceful-disconnect))

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

(define-condition malformed-packet ()
  ((m-p-packet
    :initarg :m-p-packet
    :accessor m-p-packet)
   (m-p-message
    :initarg :m-p-message
    :accessor m-p-message))
  (:documentation "Signalled when a downloaded packet is not what is expected."))

(defmethod print-object ((obj malformed-packet) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "Packet downloaded is malformed. Packet: ~S~%Message: ~A~%"
            (m-p-packet obj)
            (m-p-message obj))))

(defun signal-malformed-packet (packet &optional message)
  (error 'malformed-packet :m-p-message message :m-p-packet packet))

(define-condition parser-error ()
  ((p-e-message
    :initarg :p-e-message
    :type string
    :accessor p-e-message)
   (p-e-value
    :initarg :p-e-value
    :type t ;;take anything
    :accessor p-e-value))
  (:documentation "Signalled when an error has occurred when parsing from network"))

(defmethod print-object ((obj parser-error) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "Failed to parse.~%Message: ~A~%Value: ~A~%"
            (if (slot-boundp obj 'p-e-message)
                (p-e-message obj)
                "No message given")
            (if (slot-boundp obj 'p-e-value)
                (p-e-value obj)
                "No value given"))))

(define-condition invalid-request-line-method (parser-error)
  ())

(define-condition invalid-request-line-url (parser-error)
  ())

(define-condition invalid-request-line-version (parser-error)
  ())

(define-condition invalid-header-name (parser-error)
  ())

(define-condition invalid-header-vals (parser-error)
  ())

(define-condition end-of-headers ()
  ())

(define-condition whitespace-before-colon ()
  ())

(define-condition no-empty-serving-threads ()
  ())




