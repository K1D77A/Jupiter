(in-package :jupiter)

(defparameter *server* (make-server :port 9000))

(define-handler *server* (:GET "/greetings")
                (lambda (request response)
                  (declare (ignore request response))
                  (princ "Hello, World")))

(define-handler *server* (:GET "/")
                (lambda (request response)
                  (declare (ignore request response))
                  (princ "Hello, World")))

(defparameter *posts* ())

(define-handler *server* (:POST "/post")
                (lambda (request response)
                  (declare (ignore response))
                  (push request *posts*)
                  (princ "Hello, World")))

(defun arr-to-file (file arr)
  (with-open-file (s file :direction :output :element-type '(unsigned-byte 8))
    (dotimes (x (length arr))
      (write-byte (aref arr x) s))))

(defmethod to-arr ((fd stream));;trying to catch sb-sys:fd-stream from sbcl
  (let* ((len (file-length fd))
         (arr (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (pos len arr)
      (setf (aref arr pos) (read-byte fd)))))

(defparameter *vid-as-arr* (with-open-file
                               (s "../sausage-roll-d0.mp4" :element-type '(unsigned-byte 8))
                             (to-arr s)))

(defparameter *favicon* (with-open-file
                            (s "../favicon.ico" :element-type '(unsigned-byte 8))
                          (to-arr s)))

(define-handler *server* (:GET "/favicon.ico")
                (lambda (request response)
                  (declare (ignore request))
                  (set-content-type response
                                    (cl-mime-from-string:mime-type-from-string "../favicon.ico"))
                  (setf (body response) *favicon*)))

(define-handler *server* (:GET "/video")
                (lambda (request response)
                  (add-cookie response (make-cookie "boof" "came  l"
                                                    :domain "/video" :comment "boink"
                                                    :encode t :http-only t))
                  (add-cookie response (make-cookie "goink" "ba z  nok"
                                                    :domain "/video" :comment "boink"
                                                    :encode t :http-only t))
                  (get-cookies request)
                  ;;need to implement the lazy parsing of parameters 
                  (setf (body response) *vid-as-arr*)
                  (set-content-type response (cl-mime-from-string:mime-type-from-string
                                              "../sausage-roll-d0.mp4"))))
