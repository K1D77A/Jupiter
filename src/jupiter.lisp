;;;; jupiter.lisp

(in-package #:jupiter)

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
                     (case (car list)
                       (#\% (cons (http-char (cadr list) (caddr list))
                                  (f (cdddr list))))
                       (otherwise (cons (car list) (f (cdr list))))))))
          (coerce (f (coerce replaced 'list)) 'string))
        replaced)))

(defun parse-parameters (param-string)
  (mapcar (lambda (params)
            (let ((split (str:split "=" params)))
              (cons (intern (string-upcase (first split)))
                    (decode-parameters (second split)))))
          (str:split "&" param-string)))

(defun parse-request-line (request-line)
  (let ((words (str:words request-line)))
    (destructuring-bind (one two three)
        words
      (let ((split (str:split "?" two)))
        (list (cons :method (determine-http-method one))
              (cons :url (first split))
              (cons :parameters (parse-parameters (second split)))
              (cons :http-version three))))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\Space s))
                      (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(defun get-headers (stream)
  (loop :for line := (read-line stream)
        :for split := (str:split ":" line :limit 2)
        :do (print line)
        :while (/= (length split) 1)
        :collect
        (cons (intern (string-upcase (str:trim-left(car split))))
              (parse-header-parameters (second split)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-header-parameters content)))))

(defun parse-header-parameters (line)
  (let ((split (str:split "," line)))
    (mapcar (lambda (l)
              (mapcar #'str:trim
                      (str:split ";" l)))
            split)))

(defun serve (request-handler)
  (let ((socket (usocket:socket-listen "127.0.0.1" 8089)))
    (unwind-protect
         (loop (with-open-stream (stream (usocket:socket-stream (usocket:socket-accept socket)))
                 (let* ((url (parse-request-line (read-line stream)))
                        (path (cdr (assoc :URL url)))
                        (header (get-headers stream))
                        (params (append (cdr (assoc :PARAMETERS header))
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (usocket:socket-close socket))))

(defun hello-request-handler (path header params)
  (if (equal path "/greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ (cons header params))
            (princ (cdr name))))
      (princ "page unknown")))


