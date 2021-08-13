#lang racket

(require web-server/servlet
         web-server/servlet-env)



(define (start request)
  (response/xexpr
   '(html
     (head (title "James and his blog"))
     (body (h1 "James Blog under construction")))))


(serve/servlet start #:port 8000 #:launch-browser? #f)