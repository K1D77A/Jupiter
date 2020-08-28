(in-package :jupiter)
;;;;really should rewrite most of this to do this using binary,
;;;;that way no need to convert from octet -> char -> octet

(defparameter *header-strings-as-symbols* ())

(defun header-string->symbol (header-string)
  (check-type header-string string)
  (let ((sym (cdr (assoc header-string *header-strings-as-symbols* :test #'string=))))
    (if sym
        sym
        (progn (push (cons header-string
                           (intern (string-upcase header-string)))
                     *header-strings-as-symbols*)
               (defmethod )

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
        (case (aref string 1)
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

(defmethod parse-request-line ((http http-packet) request-line)
  (let ((words (str:words request-line)))
    (destructuring-bind (one two three)
        words
      (let ((split (str:split "?" two)))
        (with-accessors ((http-method http-method)
                         (path path)
                         (parameters parameters)
                         (http-version http-version))
            http
          (setf http-method (determine-http-method one)
                path (first split)
                parameters (parse-parameters (second split))
                http-version three)
          http)))))

(defun parse-header-parameters (line)
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



(defmethod get-headers ((http http-packet) stream)
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
  (let ((len (second (assoc 'content-length (headers http)))))
    (when len       
      (parse-integer (first len)))))


(defmethod get-content-params ((http http-packet) stream)
  ;;(push http *requests*)
  (let ((length (get-content-length http)))
    (when length
      (let ((content (make-string length)))
        (read-sequence content stream)
        (setf (body http) content
              (content-headers http)
              (parse-header-parameters content))))))

(defun parse-request (stream)
  "Given a STREAM reads from the stream and attempts to construct a valid instance of http-packet.
Finally returns this packet."
  (let ((http (make-instance 'http-packet)))
    (parse-request-line http (read-line-until-crlf stream))
    (get-headers http stream)
    (get-content-params http stream)
    http))

(defun read-line-until-CRLF (stream)
  (declare (optimize (speed 3) (safety 1)))
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







