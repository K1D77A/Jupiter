(in-package #:jupiter)

(defparameter *post1* "../src/tests/post1")
(defparameter *get1* "../src/tests/get")

(defparameter *download-header-test*
  (uiop:directory-files "../src/tests/parser-tests/headers/"))

(defparameter *download-status-line*
  (uiop:directory-files "../src/tests/parser-tests/status-line/"))

(defparameter *download-complete-packets*
  (uiop:directory-files "../src/tests/parser-tests/complete-packets/"))

(defmacro opened-file (file &body body)
  `(with-open-file (s ,file :element-type '(unsigned-byte 8))
     ,@body))

(defun read-stream-to-newline (stream)
  (loop :for byte := (read-byte stream)
        :if (= byte +newline+)
          :do (return t)))

(defun test-query-parser ()
  (loop :for x :in *download-status-line*
        :collect (opened-file x
                   (parse-request-line (make-instance 'http-packet) s))))

(defun test-download-header ()
  (loop :for x :in *download-header-test*
        :collect
        (opened-file x
          (handler-case
              (download-header s)
            (parser-error (c)
              c)
            (end-of-file (c)
              c)))))

(defun test-complete-packets ()
  (loop :for x :in *download-complete-packets*
        :collect
        (handler-case
            (opened-file x
              (parse-request s))
          (parser-error (c)
            c))))
