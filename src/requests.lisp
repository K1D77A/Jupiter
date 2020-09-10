(in-package :jupiter)
;;;;really should rewrite most of this to do this using binary,
;;;;that way no need to convert from octet -> char -> octet

(defparameter *header-strings-as-symbols* ())

(defun header-string->symbol (header-string)
  (check-type header-string string)
  (let ((sym (cdr (assoc header-string *header-strings-as-symbols* :test #'string=))))
    (if sym
        sym
        (let ((sym (intern (string-upcase header-string) :jupiter)))
          (push (cons header-string sym) *header-strings-as-symbols*)
          sym))))

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
    (when (find #\% replaced :test #'char=)
      (percent-encoding:decode replaced))))

(defun parse-parameters (param-string)
  (mapcar (lambda (params)
            (let ((split (str:split "=" params)))
              (cons (intern (string-upcase (first split)))
                    (decode-parameters (second split)))))
          (str:split "&" param-string)))

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
          (if (or (= byte 13) (= byte 10));;ignore newlines or carriage returns.
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
    (setf method (read-until-and-into-buff stream 32 method 7));;32 is Space
    (when (empty-array-p method)
      (error 'invalid-request-line-method :p-e-message "Failed in parse-request-line"))
    (setf url (read-until-and-into-buff stream 32 url 2048))
    (when (empty-array-p url)
      (error 'invalid-request-line-url :p-e-message "Failed in parse-request-line"))
    (setf http-ver (read-until-and-into-buff stream 10 http-ver 9));;10 is newline
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
  "read-bytes STREAM until UNTIL or MAX is read and puts into ARRAY. Then trims any remainder from 
ARRAY. If there is a failure then returns an empty octet-array"
  (declare (optimize (speed 3) (safety 1)))
  (let ((header-name (octet-array 512))
        (header-vals (octet-array 512)));;cap both at 512 bytes
    
    (do ((byte (read-byte stream)(read-byte stream))
         (pos 0 (1+ (the fixnum pos))))
        ((or (= 58 byte)(= pos 512));;#\: end of name
         (when (= pos 512)
           (error 'invalid-header-name
                  :p-e-message "Failed to download and parse a headers field-name in download-header"))
         (if (= (aref header-name (- pos 2)) 32);;checking if whitespace before : (i think)
             (error 'whitespace-before-colon)
             (setf header-name (subseq header-name 0 pos))))      
      (when (and (= byte 13)(zerop pos));;check for a newline
        (error 'end-of-headers));;handle the newline by signalling a condition
      (if (= byte 13)
          (decf (the fixnum pos))
          (setf (aref header-name pos) byte)))
    
    (do ((byte (read-byte stream)(read-byte stream));;don't need to worry about newline
         (pos 0 (1+ (the fixnum  pos))))
        ((or (= 10 byte) (= pos 512)) ;;end of the field val
         (when (= pos 512)
           (error 'invalid-header-vals
                  :p-e-message "Failed to download a headers field-val in download-header"))
         (setf header-vals (subseq header-vals 0 pos)))
      (if (= byte 13);;ignoring the trailing CR
          (decf (the fixnum pos))
          (setf (aref header-vals pos) byte)))

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
                     :if (= byte 13)
                       :do
                          (let ((nbyte (read-byte stream nil)))
                            (when (= nbyte 10)
                              (return boof)))))))
    (coerce list 'string)))

(defmethod get-cookies ((request http-packet))
  "Given a REQUEST this will convert every instance of the header 'Cookie' into cookie objects,
and then return them all."
  (with-accessors ((headers headers))
      request
    (flet ((to-cookie (lst)
             (let* ((vals (first lst))
                    (cookie (car vals))
                    (crumb (cdr vals)))
               (make-cookie cookie crumb :path nil :expires nil :http-only nil))))
      (let ((cookies (assoc 'Cookie headers)))
        (when cookies
          (mapcar #'to-cookie 
                  (mapcar #'parse-parameters (second cookies))))))))

(defparameter *packet* ())

(defmethod wants-to-close-p ((request http-packet))
  "Checks if a packet has sent a Connection: close header or if it hasn't sent a Connection header at
all then assume it wants to say open as per the HTTP 1.1 spec."
  (with-accessors ((headers headers))
      request
    (push request *packet*)
    (let ((con (assoc 'CONNECTION headers)))
      (string-equal (cdr con) "Close"))));;need to figure out something with the keep-alive
;;header




