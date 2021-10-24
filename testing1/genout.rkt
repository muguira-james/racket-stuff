#lang racket

(require web-server/servlet
         web-server/servlet-env
         data/queue)


(define topic-hash (make-hash))

(define (contains-topic key)
  ;; is key in this hash
  (if (member key (hash-keys topic-hash))
      #t
      #f))

(define (add-data-to-topic key data)
  ;; check to see if key is in the topic-hash and add data to the correct topic
  (if (contains-topic key)
      (enqueue! (hash-ref topic-hash key) data)
      (begin
        (let ([q (make-queue)])
          (enqueue! q data)
          (hash-set! topic-hash key q)))))

  

(define (do-nothing request)
  (response/xexpr
   '(html
     (head (title "James and his blog"))
     (body (h1 "James Blog under construction")))))

(define (hello  request)
  (response/xexpr
   '(html
     (head  (title "a blog for me"))
     (body
      (h1 "jam'n")
      (h2 "a line")))))



(provide do-nothing)
(provide hello
         add-data-to-topic
         topic-hash)