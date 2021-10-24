#lang racket

(require web-server/servlet) 
(require web-server/servlet-env)
(require json)
(require data/queue)
  ;  (require gregor)

;; a hash is structured as a topic and a queue
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

(define (remove-data-from-topic topic-name)
  (if (contains-topic topic-name)
      (begin
        (let* ([datam (dequeue! (get-queue-for-topic topic-name))])
          ;(displayln (format "::->~v" datam)
          datam))
      (begin
        (let* ([rtn (format "did not find topic ~v~%" topic-name)])
          (display rtn)
          rtn))))
        


(define (get-queue-for-topic topic-name)
  ; just return the queue for this topic,
  ;  somebody  else has to check to see if the topic-name exists
      (hash-ref topic-hash topic-name))

(define (request->jshash request)
  (string->jsexpr (bytes->string/utf-8 (request-post-data/raw request))))

(provide topic-hash
         request->jshash
         get-queue-for-topic
         remove-data-from-topic
         add-data-to-topic
         )