#lang racket
;
; -------------------------------------------
; define a simple message queue
;
;input language
;- 2 sides: the user side and  the admin side
;- user side provides commands: enqueue, dequeue, list
;- admin provides commands: topic-list, topic-count, topic-drain, topic-remove
;
; ------------- user side ----------------
;
;enqueue: format "POST" -> returns message
;{
; topic: "name"
; payload: token | list
;}
;
;dequeue: format "POST" -> returns payload in json
;{
; topic: "token"
;}
;
; topic-data: format "POST" -> returns list in json
;{
; topic: token
;}
;
; ---------- admin side -------------------
;
;topic-list: format "GET" -> returns list of topics
;
;topic-count: format "GET" -> integer
;
;
;topic-drain: format "GET" -> integer numberof items removed from that topic
;


(require web-server/servlet) 
(require web-server/servlet-env)
(require json)
(require data/queue)

;; a hash is structured as a topic and a queue
(define topic-hash (make-hash))


(define (http-response content)  
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))


(define (do-nothing request)
  ; just say nothing useful
  (http-response "nothing to see"))

(define (greeting-page request)
  ; say hi
  (http-response (list-ref '("Hi" "Hello") (random 2))))

; (define (handle-a-topic name payload)
;  ())

(define (is-member key hsh)
  ;; is key in this hash
  (if (member key (hash-keys hsh))
      #t
      #f))

(define (add-data-to-topic key data)
  ;; check to see if key is in the topic-hash and add data to the correct topic
  (if (is-member key topic-hash)
      (enqueue! (hash-ref topic-hash key) data)
      (begin
        (let ([q (make-queue)])
          (enqueue! q data)
          (hash-set! topic-hash key q)))))

(define (request->jshash request)
  (string->jsexpr (bytes->string/utf-8 (request-post-data/raw request))))

(define (enque request)
  ; put something in a queue
  ; input: { topic: "name", payload: "data-type" }
  (let* ([rtn (format "{ \"ok\" : \"that worked \" }")]
         [hsh (request->jshash request)]
         [topic-name (hash-ref hsh 'topicname)]
         [payload-data (hash-ref hsh 'payload)])
    (begin         
      (add-data-to-topic topic-name payload-data)
      (displayln
       (format
        "enq: name: ~v: data: ~v hash-size: ~v hash-keys: ~v~%"
        topic-name payload-data (hash-count topic-hash) (hash-keys topic-hash)))
      (http-response rtn))))

(define (deque request)
  (let* ([js-hsh (request->jshash request)]
         [topic-name (hash-ref js-hsh 'topicname)]
         [datam (dequeue! (hash-ref topic-hash topic-name))])
    (displayln (format "pop a datam: ~v~%" datam))
    (http-response datam)))

(define (topic-list request)
  ; show me all the topics in the topic-hash
  (let* ([rtn (format "{ \"ok\" : \"got it\" }")])
    (begin
      (let ([good-rtn (format "topics : ~v" (hash-keys topic-hash))])
        (displayln good-rtn)
        (http-response good-rtn)))))

(define (topic-count request)
  ; show me a count of topics
  ; input: { count-topics: "all" }
  (begin
    (let ([good-rtn (format "topic count : ~v" (hash-count topic-hash))])
      (displayln good-rtn)
      (http-response good-rtn))))

(define (topic-data request)
  ; list all data in a topic
  (let* ([hsh (request->jshash request)]
         [topic-name (hash-ref hsh 'topicname)])
    (begin
      (let ([rtn (format "~v: ~v ~%" topic-name (queue->list (hash-ref topic-hash topic-name)))])
        (displayln rtn)
        (http-response rtn)))))
      

(define-values (dispatch generate-url)
  ;; URL routing table (URL dispatcher).
  (dispatch-rules
   [("/") do-nothing]
   [("hello") greeting-page]  ; check to see if the service is working
   [("enque") #:method "post" enque]
   [("deque") #:method "post" deque]
   [("topic-list") topic-list]
   [("topic-count") topic-count]
   [("topic-data") #:method "post" topic-data]
   [else (error "page not found")]))



(define (request-handler request)
  (dispatch request))

;; Start the server.
(serve/servlet
  request-handler
  #:launch-browser? #f
  #:quit? #f
  ; have to listen on the  right host NOT 127.0.0.1
  #:listen-ip "0.0.0.0"
  #:port 8000
  #:servlet-regexp #rx"")


