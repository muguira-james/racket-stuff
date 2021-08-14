#lang racket

(require web-server/servlet) 
(require web-server/servlet-env)
(require json)
(require data/queue)

(define queu (make-queue))

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
  (let* ([rtn (format "{ \"ok\" : \"that worked \" }")]
         [data (bytes->string/utf-8 (request-post-data/raw request))]
         [hsh (string->jsexpr data)]
         [que-name (hash-ref hsh 'quename)]
         [que-data (hash-ref hsh 'payload)])
    (begin
      (enqueue! queu que-data)
      (displayln (format "enq-name ~v: data ~v~%" que-name que-data))
      (http-response rtn))))

(define (que-list request)
  ; show me the queue - need  to create many queues now  !!!
  ; (displayln (format "~v~%" request))
  (let* ([rtn (format "{ \"ok\" : \"got it\" }")]
         [data (bytes->string/utf-8 (request-post-data/raw request))]
         [hsh (string->jsexpr data)]
         [q-name (hash-ref hsh 'quename)])
    (begin
      (let ([good-rtn (format "queue-name : ~v, payload: ~v" q-name (queue->list queu))])
        (displayln good-rtn)
        (http-response good-rtn)))))
         
;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
   [("") do-nothing]
   [("hello") greeting-page]  ; Notice this line.
   [("enque") #:method "post" enque]
   [("queue-list") #:method "post" que-list]
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