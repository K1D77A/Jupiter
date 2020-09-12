(in-package :jupiter)
;;;;really should rewrite most of this to do this using binary,
;;;;that way no need to convert from octet -> char -> octet


(defparameter *header-strings-as-symbols* ())
(defconstant +return+ (the (integer 0 255) 13))
(defconstant +newline+ (the (integer 0 255) 10))
(defconstant +space+ (the (integer 0 255) 32))
(defconstant +colon+ (the (integer 0 255) 58))
(defconstant +htab+ (the (integer 0 255) 9))
(defconstant +equals+ (the (integer 0 255) 61))
(defconstant +plus+ (the (integer 0 255) 43))

(defconstant +field-size-restriction+ (the (integer 0 512) 512))

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

(define-condition oversized-header-field-name (parser-error)
  ())

(define-condition oversized-header-field-value (parser-error)
  ())

(define-condition ascii-delimiter-found (parser-error)
  ())

(define-condition white-space-in-field-name (parser-error)
  ())

(define-condition end-of-headers ()
  ())


(defmacro white-space-p (var)
  `(or (= ,var +space+)(= ,var +htab+)))

(defmacro ascii-delimiter-p (var)
  `(or (= ,var 28)
       (= ,var 29)
       (= ,var 30)
       (= ,var 31)))

(defmacro query-split-p (var)
  `(or (= ,var 38)
       (= ,var 59)))

(defmacro %-encoded-p (var)
  `(= ,var 37))

(defun header-string->symbol (header-string)
  (check-type header-string string)
  (locally (declare (optimize (speed 3) (safety 1)))
    (let ((sym (cdr (assoc header-string *header-strings-as-symbols* :test #'string=))))
      (if sym
          sym
          (let ((sym (intern (string-upcase header-string) :jupiter)))
            (push (cons header-string sym) *header-strings-as-symbols*)
            sym)))))

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun header-case (c)
  (check-type c character)
  (case (char-downcase c)
    (#\p :none)
    (#\g :get)
    (#\h :head)
    (#\d :delete)
    (#\c :connect)
    (#\t :trace)
    (otherwise (error "invalid request line"))))

(defun determine-http-method (string)
  (let* ((first (aref string 0))
         (key (header-case first)))
    (if (eq key :none)
        (case (char-downcase (aref string 1));;what an annoying bug...
          (#\o :post)
          (#\a :patch)
          (#\u :put))
        key)))

(defun decode-parameters (param)
  (let ((replaced (str:replace-all "+" " " param)))
    (if (find #\% replaced :test #'char=)
        (percent-encoding:decode replaced)
        replaced)))

(defun parse-parameters (param-string)
  (mapcar (lambda (params)
            (let ((split (str:split "=" params)))
              (cons (intern (string-upcase (first split)))
                    (decode-parameters (second split)))))
          (str:split "&" param-string)))

;;;want the output to look like this ((FIELD1 ("val1" "val2" "val3")(FIELD2 ...))) etc
;;;
;;;parsing each one they should set a flag to say they need to be percentage decoded

(defun reversed-char-list-to-symbol (char-list)
  (intern  (coerce (reverse char-list) 'string) :jupiter))
(defun reversed-char-list-to-string (char-list)
  (coerce (reverse char-list) 'string))

(defun download-get-query-string (stream)
  "Given a stream whose next byte is the start of a query string this will parse the string into an 
alist"
  (do ((byte (read-byte stream nil)(read-byte stream nil))
       (result-list nil)
       (write-variable-p t)
       (variable nil)
       (values-accumulator nil)
       (values nil))
      ((or (null byte)(= byte +space+))
       (if values-accumulator
           (progn 
             (push (reversed-char-list-to-string values) values-accumulator)
             (push (list (reversed-char-list-to-symbol variable) values-accumulator) result-list))
           (push (list (reversed-char-list-to-symbol variable)
                       (reversed-char-list-to-string values))
                 result-list))
       result-list)
    (cond ((= byte +equals+);;if its an = then it changes from variable to value
           (setf write-variable-p nil));; change to write to value
          ((= byte +plus+);;if its a plus then we have more values 
           (push (reversed-char-list-to-string values) values-accumulator)
           (setf values nil))
          ((query-split-p byte)
           (setf write-variable-p t)
           (if values-accumulator
               (progn 
                 (push  (reversed-char-list-to-string values) values-accumulator)
                 (push (list (reversed-char-list-to-symbol variable) values-accumulator) result-list))
               (push (list (reversed-char-list-to-symbol variable)
                           (list (reversed-char-list-to-string values)))
                     result-list))
           (setf values-accumulator nil)
           (setf variable nil)
           (setf values nil))
          (t (if write-variable-p
                 (push (char-upcase (code-char byte)) variable)
                 (push (code-char byte) values))))))
           
      
       

  (defmacro f-w-b (byte buffer)
    `(fast-io:fast-write-byte ,byte ,buffer))

(defmacro octet-array (length)
  `(fast-io:make-octet-vector ,length))

(defun read-until-and-into-buff (stream until array &optional (max 2048))
  "read-bytes STREAM until UNTIL or MAX is read and puts into ARRAY. Then trims any remainder from 
ARRAY. If there is a failure then returns an empty octet-array"
  (check-type until fast-io:octet)
  (check-type array array)
  (check-type max fixnum)
  (check-type stream stream)
  (handler-case 
      (locally
          (declare (optimize (speed 3) (safety 0));;we do type checks above
                   (type fixnum max)
                   (type fast-io:octet-vector array)
                   (type fast-io:octet until))
        (do ((byte (read-byte stream)(read-byte stream))
             (max max (1- (the fixnum max)))
             (pos (the fixnum 0) (the fixnum (1+ (the fixnum pos)))))
            ((or (= until byte) (= 0 max))
             (if (= until byte)
                 (subseq array 0 pos)
                 (octet-array 0)))
          (if (or (= byte +return+) (= byte +newline+));;ignore newlines or carriage returns.
              (decf (the fixnum pos))
              (setf (aref array pos) byte))))
    (end-of-file ()
      (octet-array 0))))

(defun empty-array-p (array)
  (zerop (length array)))

(defun octet-array-to-string (octet-vector)
  (check-type octet-vector fast-io:octet-vector)
  (locally (declare (optimize (speed 3) (safety 0)))
    (let ((var (make-string (fill-pointer octet-vector))))
      (map-into var #'code-char octet-vector)
      var)))


(defmethod parse-request-line ((http http-packet) stream)
  (declare (optimize (speed 3) (safety 1)))
  (let ((method (octet-array 7));;this is the max size for delete
        (url (octet-array 2048));;see ;;https://stackoverflow.com/questions/417142/what-is-the-maximum-length-of-a-url-in-different-browsers
        (http-ver (octet-array 9)));;this is basically the biggest
    (setf method (read-until-and-into-buff stream +space+ method 7));;32 is Space
    (when (empty-array-p method)
      (error 'invalid-request-line-method :p-e-message "Failed in parse-request-line"))
    (setf url (read-until-and-into-buff stream +space+ url 2048))
    (when (empty-array-p url)
      (error 'invalid-request-line-url :p-e-message "Failed in parse-request-line"))
    (setf http-ver (read-until-and-into-buff stream +newline+ http-ver 9));;10 is newline
    (when (empty-array-p http-ver)
      (error 'invalid-request-line-version :p-e-message "Failed in parse-request-line"))
    (with-accessors ((http-method http-method)
                     (path path)
                     (http-version http-version))
        http
      (setf http-method (determine-http-method (octet-array-to-string method))
            path (octet-array-to-string url);;currently do not handle get parameters
            ;;will need a custom read-until for the GET params
            http-version (octet-array-to-string http-ver))
      http)))

;; (defmethod parse-request-line2 ((http http-packet) request-line)
;;   (let ((words (str:words request-line)))
;;     (destructuring-bind (one two three)
;;         words
;;       (let ((split (str:split "?" two)))
;;         (with-accessors ((http-method http-method)
;;                          (path path)
;;                          (parameters parameters)
;;                          (http-version http-version))
;;             http
;;           (setf http-method (determine-http-method one)
;;                 path (first split)
;;                 parameters (parse-parameters (second split))
;;                 http-version three)
;;           (unless (keywordp http-method)
;;             (signal-malformed-packet http "http-method is not a keyword. parse-request-line"))
;;           http)))))


;;just for the fun of it, this is about 10x faster than using (str:split ..)
(defun msplit (string)
  (declare (optimize (speed 3) (safety 1)))
  (let ((str1 (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (str2 (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (flag nil))
    (loop :for char :across string 
          :do  (if (char= #\: char)
                   (setf flag t)
                   (if flag
                       (vector-push-extend char str1)
                       (vector-push-extend char str2))))
    (list (coerce str2 'string) (coerce str1 'string))))

(defun download-header (stream)
  "Given a octet STREAM attempts to download a correct http header field and its associated value.
This parser signals a variety of conditions.
If the field-name is greater than +field-size-restriction+ then signals 'oversized-header-field-name
If a newline is found as the first character then signals 'end-of-headers to tell the caller that
there headers portion of the HTTP request has finished.
If there is white-space (#\Tab or #\Space) in the header name then signals 'white-space-in-field-name
If there are any US-ascii delimiters (char-code 28 to 31) in the field name then signals 
ascii-delimiter-found.
If the field-value is greater than +field-size-restriction+ then signals 
'oversized-header-field-value.
This parser will also strip the starting optional white space from the header field value, but it 
does not remove the optional white space from the end."
  (declare (optimize (speed 3) (safety 1)))
  (let ((header-name (octet-array +field-size-restriction+))
        (header-vals (octet-array +field-size-restriction+)));;cap both at 512 bytes
    
    (do ((byte (read-byte stream)(read-byte stream))
         (pos 0 (1+ (the fixnum pos))))
        ((or (= +colon+ byte)(= pos +field-size-restriction+));;#\: end of name
         (when (= pos +field-size-restriction+)
           (error 'oversized-header-field-name
                  :p-e-message "Failed to download and parse a headers field-name in download-header"))
         (setf header-name (subseq header-name 0 pos)))
      (when (ascii-delimiter-p byte)
        (error 'ascii-delimiter-found
               :p-e-message "Found an ascii delimiter in field-name in download-header"))
      (when (and (or (= byte +newline+)
                     (= byte +return+))
                 (zerop pos));;check for a newline
        (error 'end-of-headers));;handle the newline by signalling a condition
      (when (white-space-p byte)
        (error 'white-space-in-field-name
               :p-e-message "white-space found in field-name in download-header"))
      (if (= byte +return+)
          (decf (the fixnum pos))
          (setf (aref header-name pos) byte)))
    
    (do ((byte (read-byte stream)(read-byte stream));;don't need to worry about newline
         (pos 0 (1+ (the fixnum  pos)))
         (start (the fixnum 0))
         (hit-non-whitespace-p nil))
        ((or (= +newline+ byte) (= pos +field-size-restriction+)) ;;end of the field val
         (when (= pos +field-size-restriction+)
           (error 'oversized-header-field-value
                  :p-e-message "Failed to download a headers field-val in download-header"))
         (setf header-vals (subseq header-vals start pos)))
      (when (/= byte +space+);;if we hit a non whitespace char set to t
        (setf hit-non-whitespace-p t))
      (if (= byte +return+);;ignoring the trailing CR
          (decf (the fixnum pos))
          (setf (aref header-vals pos) byte))
      (when (and (not hit-non-whitespace-p) (white-space-p byte))
        (incf (the fixnum start))));;want to remove a space that occurs at the start.
    
    (cons (header-string->symbol (octet-array-to-string header-name))
          (octet-array-to-string header-vals))))

(defmethod parse-headers ((http http-packet) stream)
  (let ((headers ()))
    (handler-case
        (loop :do (let ((header (download-header stream)))
                    (push header headers)))
      (end-of-headers ()
        (setf (headers http) headers)))
    http))

(defmethod get-headers2 ((http http-packet) stream)
  (setf (headers http)
        (loop :for line := (read-line-until-crlf stream)
              :for split := (if (= 1 (length line))
                                '("")
                                (msplit line))
              :while (/= (length split) 1)
              :collect
              (cons (header-string->symbol (str:trim-left (car split)))
                    (parse-header-parameters (second split)))))
  http)

(defparameter *requests* ())

(defun parse-header-parameters (line)
  (unless (stringp line)
    (let ((newline (make-array (length line) :element-type 'character)))
      (map-into newline #'code-char line)
      (setf line newline)))
  (let ((split (str:split "," line)))
    (mapcar (lambda (l)
              (mapcar #'str:trim
                      (str:split ";" l)))
            split)))

(defmethod get-content-length ((http http-packet))
  (let ((len (cdr (assoc 'content-length (headers http)))))
    (when len       
      (parse-integer (first len)))))

(defmethod get-content-params ((http http-packet) stream)
  ;;(push http *requests*)
  (let ((length (get-content-length http)))
    (when length
      (let ((content (make-array length :element-type '(unsigned-byte 8))))
        (read-sequence content stream)
        (setf (body http) content
              (content-headers http)
              (parse-header-parameters content))))))

(defun parse-request (stream)
  "Given a STREAM reads from the stream and attempts to construct a valid instance of http-packet.
Finally returns this packet."
  (let ((http (make-instance 'http-packet)))
    (parse-request-line http stream)
    (parse-headers http stream)
    (get-content-params http stream)
    http))

(defun read-line-until-CRLF (stream)
  (declare (optimize (speed 3) (safety 1)))
  (unless (open-stream-p stream)
    (signal-dirty-disconnect stream))
  (let ((list 
          (the list
               (loop :for byte := (read-byte stream nil)
                     :collect (code-char byte) :into boof 
                     :if (= byte +return+)
                       :do
                          (let ((nbyte (read-byte stream nil)))
                            (when (= nbyte +newline+)
                              (return boof)))))))
    (coerce list 'string)))


(defparameter *packet* ())

(defmethod wants-to-close-p ((request http-packet))
  "Checks if a packet has sent a Connection: close header or if it hasn't sent a Connection header at
all then assume it wants to say open as per the HTTP 1.1 spec."
  (with-accessors ((headers headers))
      request
    (push request *packet*)
    (let ((con (assoc 'CONNECTION headers)))
      (string-equal (cdr con) "Close"))))




