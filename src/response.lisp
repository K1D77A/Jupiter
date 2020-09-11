(in-package :jupiter)
;;;;this file contains the steps taken to construct a valid response object.

(defparameter *status-codes*
  '((:100 . "100 Continue") (:101 . "101 Switching Protocols")
    (:102 . "102 Processing") (:200 . "200 OK") (:201 . "201 Created")
    (:202 . "202 Accepted") (:203 . "203 Non-authoritative Information")
    (:204 . "204 No Content") (:205 . "205 Reset Content")
    (:206 . "206 Partial Content") (:207 . "207 Multi-Status")
    (:208 . "208 Already Reported") (:226 . "226 IM Used")
    (:300 . "300 Multiple Choices") (:301 . "301 Moved Permanently")
    (:302 . "302 Found") (:303 . "303 See Other")
    (:304 . "304 Not Modified") (:305 . "305 Use Proxy")
    (:307 . "307 Temporary Redirect") (:308 . "308 Permanent Redirect")
    (:400 . "400 Bad Request") (:401 . "401 Unauthorized")
    (:402 . "402 Payment Required") (:403 . "403 Forbidden")
    (:404 . "404 Not Found") (:405 . "405 Method Not Allowed")
    (:406 . "406 Not Acceptable") (:407 . "407 Proxy Authentication Required")
    (:408 . "408 Request Timeout") (:409 . "409 Conflict")
    (:410 . "410 Gone") (:411 . "411 Length Required")
    (:412 . "412 Precondition Failed") (:413 . "413 Payload Too Large")
    (:414 . "414 Request-URI Too Long") (:415 . "415 Unsupported Media Type")
    (:416 . "416 Requested Range Not Satisfiable")
    (:417 . "417 Expectation Failed") (:418 . "418 I'm a teapot")
    (:421 . "421 Misdirected Request") (:422 . "422 Unprocessable Entity")
    (:423 . "423 Locked") (:424 . "424 Failed Dependency")
    (:426 . "426 Upgrade Required") (:428 . "428 Precondition Required")
    (:429 . "429 Too Many Requests")
    (:431 . "431 Request Header Fields Too Large")
    (:444 . "444 Connection Closed Without Response")
    (:451 . "451 Unavailable For Legal Reasons")
    (:499 . "499 Client Closed Request") (:500 . "500 Internal Server Error")
    (:501 . "501 Not Implemented") (:502 . "502 Bad Gateway")
    (:503 . "503 Service Unavailable") (:504 . "504 Gateway Timeout")
    (:505 . "505 HTTP Version Not Supported")
    (:506 . "506 Variant Also Negotiates") (:507 . "507 Insufficient Storage")
    (:508 . "508 Loop Detected") (:510 . "510 Not Extended")
    (:511 . "511 Network Authentication Required")
    (:599 . "599 Network Connect Timeout Error")))

(defun code->status (code)
  (check-type code keyword)
  (cdr (assoc code *status-codes*)))

(defun set-content-type (response new-content-type)
  (check-type new-content-type string)
  (setf (second (assoc "Content-Type" (headers response) :test #'string-equal))
        new-content-type))

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

(defmethod add-cookie ((response http-response) (cookie cookie))
  "Given a RESPONSE object this will add a Set-Cookie header to 
the response object using COOKIE as its value."
  (with-accessors ((headers headers))
      response
    (setf headers
          (append headers (list (list "Set-Cookie" cookie))))))
;;;need to encode any spaces... https://en.wikipedia.org/wiki/Percent-encoding
;;;well don't have to can just tell the user than no effort is made to perform percent encoding

(defun send-response (stream response)
  "Given a STREAM and a RESPONSE object this function will serialize RESPONSE and send it down STREAM."
  (unless (open-stream-p stream)
    (signal-dirty-disconnect stream))
  (let* ((headers (headers response))
         (nstream (make-string-output-stream))
         (body (body response))
         (body-as-string)
         (body-len
           (if (arrayp body)
               (length body)
               (progn (setf body-as-string (get-output-stream-string (body response)))
                      (length body-as-string))))
         (*print-readably* t));;remove #< from printed representation of response
    (setf (headers response) (append headers (list (list "Content-Length" body-len))))
    (print-object response nstream);;this will print the headers into the new stream
    (when body-as-string 
      (response-format nstream "~A" body-as-string));;only print the body if its a string
    (force-output nstream)
    (let ((str (get-output-stream-string nstream)))
      (write-sequence (string-to-octets str) stream)
      (unless body-as-string ;;if the body wasn't a string then finally send that
        (write-sequence body stream))))
  (force-output stream))

(defun string-to-octets (string)
  (check-type string string)
  (locally (declare (optimize (speed 3) (safety 1)))
    (let ((ar (make-array (length (the string string)) :element-type '(unsigned-byte 8))))
      (map-into ar #'char-code string))))


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
