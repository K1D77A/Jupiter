(in-package :jupiter)

(defparameter *server* (make-server :port 9001))

(define-handler *server* (:GET "/greetings")
                (lambda (request response)
                  (declare (ignore request response))
                  (princ "beep")))

(defun arr-to-file (file arr)
  (with-open-file (s file :direction :output :element-type '(unsigned-byte 8))
    (dotimes (x (length arr))
      (write-byte (aref arr x) s))))

(defmethod to-arr ((fd stream));;trying to catch sb-sys:fd-stream from sbcl
  (let* ((len (file-length fd))
         (arr (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (pos len arr)
      (setf (aref arr pos) (read-byte fd)))))

(define-handler *server* (:GET "/video")
                (lambda (request response)
                  (declare (ignore request))
                  (with-open-file (s "../are-abbos-even-human-d0.mp4" :element-type '(unsigned-byte 8))
                    (setf (body response) (to-arr s)))
                  (set-content-type response (cl-mime-from-string:mime-type-from-string
                                              "../are-abbos-even-human-d0.mp4"))))
