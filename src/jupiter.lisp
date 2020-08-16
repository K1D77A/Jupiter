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

(defun decode-param (s)
  (labels ((f (list)
             (when list
               (case (car list)
                 (#\% (cons (http-char (cadr list) (caddr list))
                            (f (cdddr list))))
                 (#\+ (cons #\Space (f (cdr list))))
                 (otherwise (cons (car list) (f (cdr list))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun header-case (c)
  (check-type c character)
  (case c
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

(defun parse-request-line (request-line)
  (let ((words (str:words request-line)))
    (destructuring-bind (one two three)
        words
      (list (cons :method (determine-http-method one))
            (cons :url two)
            (cons :http-version three)))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\Space s))
                      (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (Make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(defun serve (request-handler)
  (let ((socket (usocket:socket-listen "127.0.0.1" 8080)))
    (unwind-protect
         (loop (with-open-stream (stream (usocket:socket-stream (usocket:socket-accept socket)))
                 (let* ((url (parse-url (read-line stream)))
                        (path (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (usocket:socket-close socket))))

(defun hello-request-handler (path header params)
  (declare (ignore header))
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ "bazoink")
            (princ (cdr name))))
      (princ "page unknown")))


