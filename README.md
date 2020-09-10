# Jupiter

Currently has support for HTTP 1.1 persistent connections. 



This is just a trivial HTTP server that I have made for the fun of it.
My aim for Jupiter is that It's very user friendly. Here is a small example: 
```lisp
(define-handler *server* (:GET "/greetings")
                (lambda (request response)
                  (declare (ignore request response))
                  (princ "beep")))
```
The small example simply serves the content "beep" on the path '/greetings'.
```lisp
(defparameter *vid-as-arr* (with-open-file
                               (s "../sausage-roll-d0.mp4" :element-type '(unsigned-byte 8))
                             (to-arr s)))

(define-handler *server* (:GET "/video")
                (lambda (request response)
                  (add-cookie response (make-cookie "boof" "came  l"
                                                    :domain "/video" :comment "boink"
                                                    :encode t :http-only t))
                  (add-cookie response (make-cookie "goink" "ba z  nok"
                                                    :domain "/video" :comment "boink"
                                                    :encode t :http-only t))
                  (get-cookies request)
                  (setf (body response) *vid-as-arr*)
                  (set-content-type response (cl-mime-from-string:mime-type-from-string
                                              "../sausage-roll-d0.mp4"))))
```
This example is more complicated, this sets two custom made cookies with some flags (the :encode 
flag means that in the cookie and crumb are percentage encoded), 
it shows how to receive the cookies sent on a request, how to send binary data to the client
and how to set the content type of your response.

Each handler has two variables passed to it, a 'request' object which is obviously a CL object that
attempts to represent the request made to the server and a 'response' object which the user can 
manipulate ie content-type, the body of the response, add cookies etc to serve up the content they wish. 

Although I have implemented cookies and the implementation is ready to support sessions I haven't
added a session system yet. I also haven't tested post requests etc although I know that post 
parameters are passed correctly and put into (parameters request) the same is true for get requests.



## License

MIT

