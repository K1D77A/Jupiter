;;;; package.lisp

(defpackage #:jupiter
  (:use #:cl)
  (:export #:define-handler
           ;;cookies
           #:add-cookie
           #:get-cookie
           #:make-cookie
           #:cookie
           #:crumb
           #:domain
           #:path
           #:make-session-p
           #:expires
           #:comment
           #:encodep
           #:http-only
           
           ;;conditions
           ;;no-associated-handler
           #:no-associated-handler
           #:n-a-h-url
           #:n-a-h-http-method

           ;;server
           #:port
           #:interface
           #:handlers
           #:make-server
           ;;http-response
           #:http-version
           #:status-code
           #:headers
           #:body
           #:set-content-type

           ;;http-packet ie a request
           #:http-version
           #:method
           #:path
           #:parameters
           #:headers
           #:content-headers
           #:body
           ))
