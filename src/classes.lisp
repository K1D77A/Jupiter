(in-package :jupiter)


(defclass special-slot (c2mop:standard-direct-slot-definition)
  ())


(defparameter *slots-needed* '(ACCEPT ACCEPT-ENCODING))
(defparameter *direct-slots* (mapcar (lambda (sym)
                                       (make-instance 'special-slot
                                                      :initargs (list :readers sym)
                                                      :name sym
                                                      ))
                                     *slots-needed*))

(defmethod c2mop:validate-superclass ((class special-slot) (metaclass standard-class))
  t)


(defclass request-meta (c2mop:standard-class)
  ())

(defmethod c2mop:validate-superclass ((class request-meta) (metaclass standard-class))
  t)

(defmethod c2mop:compute-effective-slot-definition ((class special-slot) name dslots)
  (declare (ignore name dslots))
  (let ((slot (call-next-method)))
    slot))

(defmethod make-instance ((class request-meta) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method class initargs))

(defmethod c2mop:compute-slots ((class request-meta))
  (append (call-next-method)
          *direct-slots*))

(defclass request ()
  ((header-symbols
    :accessor header-symbols
    :initarg :sym)
   (boofta
    :initarg :boofta))
  (:metaclass request-meta))


(defun make-request-class ()
  (c2cl:ensure-class )




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


(defun make-http-response (server handler status-code)
  (let ((headers (list (list "Date" (time-now))
                       (list "Server" (server-version server))
                       (list "Last-Modified" (last-modified handler))
                       (list "Connection" "keep-alive")
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
   (%in-use
    :accessor in-use
    :initform nil
    :type boolean)
   (%con-stream;;currently not used not sure if keeping a stream around is a better idea or not 
    :accessor con-stream
    :initform nil
    :initarg :con-stream))
  (:metaclass metalock:metalock))

(defun grab-incoming-connection (incoming-connection)
  "Returns either nil or the incoming-connection, if nil is returned it means that the 
incoming-connection is in use already."
  (unless (in-use incoming-connection)
    (setf (in-use incoming-connection) t)
    incoming-connection))

(defun release-incoming-connection (incoming-connection)
  (setf (in-use incoming-connection) nil)
  t)



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
   (%connections
    :accessor connections
    :type list
    :initform '())
   (%interface
    :accessor interface
    :initarg :interface
    :initform "0.0.0.0")
   (%handlers
    :accessor handlers
    :initform (make-handlers-hash))
   (%serving-thread
    :accessor serving-thread)
   (%con-receive-thread
    :accessor con-receive-thread)
   (%listening-socket
    :accessor listening-socket
    :initarg :listening-socket))
  (:metaclass metalock:metalock))

(defun make-server (&key (port 8080) (interface "0.0.0.0"))
  (let ((server (make-instance 'server
                               :port port :interface interface
                               :listening-socket (usocket:socket-listen interface port
                                                                        :reuse-address t
                                                                        :reuseaddress t))))
    (setf (con-receive-thread server)
          (bt:make-thread (lambda () (get-connections server))))
    ;; (serving-thread server)
    ;; (bt:make-thread
    ;;  (lambda () (loop (loop :for x :in (connections server)
    ;;                    :do (service-incoming-connection server x))))))
    server))

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
