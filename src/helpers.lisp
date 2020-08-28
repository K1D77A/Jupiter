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

(defmacro add-setter-for-http-packet (symbol)
  "Given a symbol generates what is effectively an anonymous setter for that symbol within 
http-packets' 'headers' slot which is an alist."
  (let ((setter (gensym)))
    (format *standard-output* "~&adding setter for header: ~A~%" symbol)
    `(progn (defmethod ,setter ((http-packet http-packet) new-val)
              (second (rplacd (assoc ',symbol (headers http-packet) :test #'eq) (list new-val))))
            ',setter)))

(defmacro add-getter-for-http-packet (symbol)
  (format *standard-output* "~&adding a getter function by the name of '~A' to (headers http-packet)"
          symbol)
  `(progn (defmethod ,symbol ((http-packet http-packet))
            (second (assoc ',symbol (headers http-packet) :test #'eq)))
          ',symbol))

(defmacro add-setfer-for-http-packet (symbol setter-symbol)
  (format *standard-output* "~&adding a setf for the getter function named by ~A.~%" symbol)
  `(defsetf ,symbol ,setter-symbol))

(defmacro test-setter-getters ()
  `(let ((setter-sym (add-setter-for-http-packet age)))
     (add-getter-for-http-packet age)
     (add-setfer-for-http-packet age setter-sym)))
;;;this is another dead end, I think a better idea is to use (define-handler ..) to expand calls to
;;;certain functions like '(age request)' to (second (assoc 'age (headers request) :test #'eq)) etc
;;;and expand a (setf (age request) "zonk") to
;;;(setf (second (assoc 'age (headers request) :test #'eq)) "zonk")
;;;and do that by storing an association between certain symbols and expansions. could be fun
