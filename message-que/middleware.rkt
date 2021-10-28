#lang racket

(require web-server/servlet) 
(require web-server/servlet-env)
(require json)
(require data/queue)
  ;  (require gregor)

;; a hash is structured as a topic and a queue
(define topic-hash (make-hash))

;(require "topic-store.rkt")


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
      ; we have the topic, test the queue
      (if (queue-empty? (get-queue-for-topic topic-name))
          ; queue has something in it
          (let* ([rtn (format "{ \"error\": \"no data in queue ~v\" }~%" topic-name)])
            (displayln rtn)
            rtn)
          ; queue has nothing in it.
          (format  "{ \"topic-name\": ~s \"payload\": ~v }"
                   topic-name (dequeue! (get-queue-for-topic topic-name)))

          )
      ; we don't have the input topic, build an error and return that
      (begin
        (let* ([rtn (format "{ \"error\": \"did not find topic ~v~%" topic-name)])
          (display rtn)
          rtn))))

(define (get-queue-for-topic topic-name)
  ; just return the queue for this topic,
  ;  somebody  else has to check to see if the topic-name exists
  (if (contains-topic topic-name)
      (hash-ref topic-hash topic-name)
      (make-queue)))

(define (request->jshash request)
  (string->jsexpr (bytes->string/utf-8 (request-post-data/raw request))))

(provide topic-hash
         request->jshash
         get-queue-for-topic
         remove-data-from-topic
         add-data-to-topic
         contains-topic
         )