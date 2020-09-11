(in-package :jupiter)

(defun response-format (destination control-string &rest format-arguments)
  "Just a normal 'format' but after it appends a carriage return and a newline."
  (apply #'format destination control-string format-arguments)
  (format destination "~A~%" #\Return))

(defun key-val-to-string (key val)
  (format nil "~A=~A" key val))

(defparameter *http-rfc1123-timezome* (butlast local-time:+rfc-1123-format+))

(defun time-now ()
  (format nil "~A~A" (local-time:format-timestring nil (local-time:now)
                                                   :format *http-rfc1123-timezome*
                                                   :timezone local-time:+utc-zone+)
          "GMT"))

(defun time+ (n unit)
  (let ((time (local-time:timestamp+ (local-time:now) n unit)))
    (format nil "~A~A" (local-time:format-timestring nil time 
                                                     :format *http-rfc1123-timezome*
                                                     :timezone local-time:+utc-zone+)
            "GMT")))

(defmacro timeout-body ((time endval) &body body)
  `(handler-case (bt:with-timeout (,time)
                   ,@body)
     (condition ()
       ,endval)))

(defun clean-string (string)
  "removes #\Space 's from the start of a string"
  (subseq string (position-if-not (lambda (c) (char= #\Space c)) string)))
