(in-package :jupiter)

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
    (if (str:containsp "%" replaced)
        (labels ((f (list)
                   (when list
                     (if (char= (car list) #\%)
                         (cons (http-char (cadr list) (caddr list))
                               (f (cdddr list)))
                         (cons (car list) (f (cdr list)))))))
          (coerce (f (coerce replaced 'list)) 'string))
        replaced)))

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

(defmethod get-headers ((http http-packet) stream)
  (setf (headers http)
        (loop :for line := (read-line stream)
              :for split := (str:split ":" line :limit 2)
              :do (print line)
              :while (/= (length split) 1)
              :collect
              (cons (intern (string-upcase (str:trim-left (car split))))
                    (parse-header-parameters (second split)))))
  http)

(defmethod get-content-params ((http http-packet) stream)
  (let ((length (cdr (assoc 'content-length (headers http)))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (setf (body http) content
              (content-headers http)
              (parse-header-parameters content))))))

(defun parse-request (stream)
  "Given a STREAM reads from the stream and attempts to construct a valid instance of http-packet.
Finally returns this packet."
  (let ((http (make-instance 'http-packet)))
    (parse-request-line http (read-line stream))
    (get-headers http stream)
    (get-content-params http stream)
    http))
