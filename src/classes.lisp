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

(defun make-handlers-hash ()
  (let ((hash-table (make-hash-table :test #'eq)))
    (mapcar (lambda (method)
              (setf (gethash method hash-table)
                    (make-hash-table :test #'equalp)))
            *valid-methods*)
    hash-table))

(defclass server ()
  ((%port
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
