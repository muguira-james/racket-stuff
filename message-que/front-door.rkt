#lang racket
;
; -------------------------------------------
; define a simple message queue
;
; - updates required:
;   1. catch exceptions
;   2. do a better job checking the input  request
;   3. reply with json  - DONE!
;   4. add tests of the low level queue code (not the http side)
;
;input language
;- 2 sides: the user side and  the admin side
;- user side provides commands: enqueue, dequeue, list
;- admin provides commands: topic-list, topic-count, topic-drain, topic-remove
;
; ------------- user side ----------------
;
;enqueue: format "POST" -> returns message
;{  topic: "name"  payload: token | list}
;
;dequeue: format "POST" -> returns payload in json
;{ topic: "token" }
;
; topic-data: format "POST" -> returns list in json
;{ topic: token }
;
; queue-drain: format 'POST" -> return message
; { topic-name: token }
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
  ;  (require gregor)

(require "middleware.rkt")


(define (hello request)
  
  (http-response (format "hello: today")))

(define (do-nothing request)
  ; just say nothing useful
  (http-response "<div>welcome to <span style=\"color:blue\">jamQ</span></div>"))

(define (greeting-page request)
  ; say hi
  (http-response (list-ref '("Hi" "Hello") (random 2))))


(define (enque request)
  ; put something in a queue
  ; input: { topic: "name", payload: "data-type" }
  (let* ([hsh (request->jshash request)]
         [topic-name (hash-ref hsh 'topicname)]
         [payload-data (hash-ref hsh 'payload)])
    (begin         
      (add-data-to-topic topic-name payload-data)
    
      (let ([rtn (make-hash)])
        (build-json-response rtn 'cmd "enque")
        (build-json-response rtn 'topic-name topic-name)
        (build-json-response rtn 'data payload-data)

        (displayln (with-output-to-string (lambda () (write-json  rtn))))
        (http-response (with-output-to-string (lambda  () (write-json rtn))))))))

(define (deque request)
  ; check if topic exists, remove 1st item from topic queue
  (let* ([js-hsh (request->jshash request)]
         [topic-name (hash-ref js-hsh 'topicname)]
         [rtn (make-hash)]
         [rtn (remove-data-from-topic topic-name)])
    (begin
      (displayln (with-output-to-string (lambda () (write-json rtn))))
      (http-response (with-output-to-string (lambda  () (write-json rtn)))))))


(define (topic-list request)
  ; show me all the topics in the topic-hash
  (begin
    (let* ([rtn (make-hash)])
      (build-json-response rtn 'topic-list (hash-keys topic-hash))
      (displayln (with-output-to-string (lambda () (write-json rtn))))
      (http-response (with-output-to-string (lambda() (write-json rtn)))))))

(define (topic-count request)
  ; show me a count of topics
  ; input: { count-topics: "all" }
  (begin
    (let ([rtn (make-hash)])
      (build-json-response rtn 'topic-count (hash-count topic-hash))
      (displayln (with-output-to-string (lambda () (write-json rtn))))
      (http-response (with-output-to-string (lambda () (write-json rtn)))))))

(define (topic-data request)
  ; list all data in a topic
  (let* ([hsh (request->jshash request)]
         [topic-name (hash-ref hsh 'quename)]
         [rtn (make-hash)])
        (build-json-response rtn 'topic-name topic-name)
        (build-json-response rtn 'topic-list (queue->list (hash-ref topic-hash topic-name)))
        (displayln (with-output-to-string (lambda () (write-json rtn))))
        (http-response (with-output-to-string (lambda () (write-json rtn))))))

(define (drain-queue request)
  (let* ([hsh (request->jshash request)]
         [topic-name (hash-ref hsh 'quename)]
         [rtn (make-hash)])
    (hash-set! topic-hash topic-name (make-queue))
    (build-json-response rtn 'cmd "drain-queue")
    (build-json-response rtn 'topic-name topic-name)
    (build-json-response rtn 'message (format "drain-queue: ~v: all data deleted" topic-name)
    (displayln (with-output-to-string (lambda () (write-json rtn))))
    (http-response (with-output-to-string (lambda ()  (write-json  rtn)))))))

(define (topic-remove request)
  (let* ([hsh  (request->jshash request)]
         [topic-name (hash-ref hsh 'quename)]
         [rtn (make-hash)])
    (hash-remove! topic-hash topic-name)
    (build-json-response rtn 'message (format "topic-remove: removed topic ~v" topic-name))
    (displayln (with-output-to-string (lambda  () (write-json rtn))))
    (http-response (with-output-to-string (lambda () (write-json rtn))))))

(define (build-json-response hsh key value)
  (hash-set! hsh key value))

(define (http-response content)  
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))


(define-values (dispatch generate-url)
  ;; URL routing table (URL dispatcher).
  (dispatch-rules
   [("") do-nothing]
   [("hello") hello]  ; check to see if the service is working
   [("enque") #:method "post" enque]
   [("deque") #:method "post" deque]
   [("topic-list") topic-list]
   [("topic-count") topic-count]
   [("topic-data") #:method "post" topic-data]
   [("drain-queue") #:method "post" drain-queue]
   [("topic-remove") #:method "post" topic-remove]
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


