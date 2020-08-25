(in-package :jupiter)

(defun response-format (destination control-string &rest format-arguments)
  "Just a normal 'format' but after it appends a carriage return and a newline."
  (apply #'format destination control-string format-arguments)
  (format destination "~A~%" #\Return))
