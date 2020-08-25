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
   (%response-array
    :type array
    :initform #()
    :accessor response-array
    :documentation "this array is optional, if you set this, this will be sent to the client 
instead of body")
   (%headers
    :type list
    :initarg :headers
    :accessor headers)
   (%body
    :type stream
    :initform (make-string-output-stream)
    :accessor body))
  (:documentation "A class used to create the headers for a response that are sent to a requester"))


(defun make-http-response (server handler status-code)
  (let ((headers (list (list "Date" (time-now))
                       (list "Server" (server-version server))
                       (list "Last-Modified" (last-modified handler))
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
   (%response-thread
    :accessor response-thread)
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
           (loop :for lst :in (headers obj)
                 :do (response-format stream "~A: ~A" (first lst) (second lst)))
           (response-format stream "")));;important to add a CRLF before body
    (if *print-readably*
        (fun)
        (print-unreadable-object (obj stream)
          (fun)))))

