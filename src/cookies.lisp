(in-package :jupiter)



(defclass cookie ()
  ((%cookie
    :initarg :cookie 
    :accessor cookie)
   (%crumb
    :initarg :crumb 
    :accessor crumb)
   (%domain
    :initarg :domain
    :accessor domain)
   (%path
    :initform "/"
    :initarg :path
    :accessor path)
   (%make-session-p
    :initform nil
    :initarg :make-session-p
    :accessor make-session-p
    :documentation "If this is true then expires is ignored and this cookie becomes a session cookie")
   (%expires
    :initarg :expires
    :initform nil
    :accessor expires)
   (%comment
    :initarg :comment
    :initform nil
    :accessor comment)
   (%encodep
    :initarg :encodep
    :initform nil
    :accessor encodep
    :documentation "When set to t this will mean that cooking and crumb are percent encoded.")
   (%http-only
    :initarg :http-only
    :initform t
    :accessor http-only)))




;;;also need to have a Secure header at some point but no support for HTTPS yet

(defun make-cookie (key val &key (path "/") (make-session-p nil) (expires 7) (http-only t)
                              (domain nil) (comment nil) (encode nil)) 
  (make-instance 'cookie :http-only http-only
                         :expires (if expires
                                      (time+ expires :day)
                                      nil)
                         :encodep encode
                         :make-session-p make-session-p
                         :path path
                         :domain domain
                         :comment comment
                         :crumb val
                         :cookie key))

(defmethod print-object ((obj cookie) stream)
  ;;gotta convert cookie from an object to something like
  ;;lu=Rg3vHJZnehYLjVg7qi3bZjzg; Expires=Tue, 15 Jan 2013 21:47:38 GMT; Path=/; Domain=.example.com; ;;HttpOnly
  (flet ((fun ()
           (with-accessors ((cookie cookie)
                            (crumb crumb)
                            (path path)
                            (make-session-p make-session-p)
                            (expires expires)
                            (http-only http-only)
                            (comment comment)
                            (encodep encodep))
               obj
             (format stream "~A=~A; "
                     (if encodep
                         (percent-encoding:encode cookie)
                         cookie)
                     (if encodep
                         (percent-encoding:encode crumb)
                         crumb));;Cookie=Crumb;
             (when (and expires (not make-session-p))
               (format stream "Expires=~A; " expires))
             (when path
               (format stream "Path=~A; " path))
             (when comment
               (format stream "Comment=~A; " comment))
             (when http-only
               (format stream "HttpOnly")))))
    (if *print-readably*
        (fun)
        (print-unreadable-object (obj stream)
          (fun)))))


(defmethod get-cookies ((request http-packet))
  "Given a REQUEST this will convert every instance of the header 'Cookie' into cookie objects,
and then return them all in a list. It will also change the REQUEST objects instance of cookie,
initially it will be a string that needs to be parsed, but after calling this the first time
they are parsed and changed into the new parsed version. Subsequent calls to this function just
evaluate to (cdr (assoc 'Cookie (headers REQUEST)))"
  ;;cookie should only be sent once "MUST NOT send more than one cookie header"
  ;;if for some reason a useragent does sent more than one then this will just convert the first
  ;;instance of assoc 'Cookie
  (with-accessors ((headers headers))
      request    
    (labels ((to-cookie (lst)
               (let* ((vals (first lst))
                      (cookie (car vals))
                      (crumb (cdr vals)))
                 (make-cookie cookie crumb :path nil :expires nil :http-only nil)))
             (parse-first (cookie-string)
               (let ((cookies (first (parse-header-parameters cookie-string))))
                 (when cookies
                   (let ((as-obj (mapcar #'to-cookie 
                                         (mapcar #'parse-parameters cookies))))
                     (rplacd (assoc 'Cookie headers) (list as-obj));;change them permanently 
                     as-obj)))))
      (let ((cookie (cdr (assoc 'Cookie headers))))
        (typecase cookie
          (list cookie)
          (t (parse-first cookie)))))))
