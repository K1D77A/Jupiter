(in-package #:jupiter)

(defparameter *post1* "../src/tests/post1")
(defparameter *get1* "../src/tests/get")

(defmacro o-f (file &body body)
  `(with-open-file (s ,file :element-type '(unsigned-byte 8))
     ,@body))


