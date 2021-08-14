#lang racket

(require web-server/servlet) 
(require web-server/servlet-env)
(require json)
(require data/queue)

(define topic-hash (make-hash))
; (define queu (make-queue))

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


(define (enque request)
  ; put something in a queue
  ; input: { topic: "name", payload: "data-type" }
  (let* ([rtn (format "{ \"ok\" : \"that worked \" }")]
         [data (bytes->string/utf-8 (request-post-data/raw request))]
         [hsh (string->jsexpr data)]
         [que-name (hash-ref hsh 'quename)]
         [que-data (hash-ref hsh 'payload)])
    (begin
      (let ([q (make-queue)])
        (enqueue! q que-data)
        (hash-set! topic-hash que-name q))
      
      (displayln
       (format "enq: name ~v: data ~v hash-size ~v hash-keys ~v~%" que-name que-data (hash-count topic-hash) (hash-keys topic-hash)))
      (http-response rtn))))

(define (topic-list request)
  ; show me all the topics
  ; input: { list-topics: "all"  }
  ; (displayln (format "~v~%" request))
  (let* ([rtn (format "{ \"ok\" : \"got it\" }")]
         )
    (begin
      (let ([good-rtn (format "topics : ~v" (hash-keys topic-hash))])
        (displayln good-rtn)
        (http-response good-rtn)))))

(define (topic-count requests)
  ; show me a count of topics
  ; input: { count-topics: "all" }
  (begin
    (let ([good-rtn (format "topic count : ~v" (hash-count topic-hash))])
      (displayln good-rtn)
      (http-response good-rtn))))

;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
   [("") do-nothing]
   [("hello") greeting-page]  ; Notice this line.
   [("enque") #:method "post" enque]
   [("topic-list") topic-list]
   [("topic-count") topic-count]
   [else (do-nothing)]))



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



;> (require json)
;> (string->jsexpr "{ \"foo\": 42 }")
;'#hasheq((foo . 42))