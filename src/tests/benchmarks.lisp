(in-package :jupiter)

(defparameter *wrk10* '("wrk" "-c 10" "-t 4" "-d 10" "http://127.0.0.1:9000"))
(defparameter *wrk100* '("wrk" "-c 100" "-t 4" "-d 10" "http://127.0.0.1:9000"))


(defun benchmark ()
  (let ((ten (make-string-output-stream))
        (one (make-string-output-stream)))
    (format t "Benchmarking 10 connections 4 threads~%")
    (uiop:run-program *wrk10* :output ten)
    (format t "Benchmarking 100 connections 4 threads~%")
    (uiop:run-program *wrk100* :output 100)))
