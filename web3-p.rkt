#lang racket

(require web-server/servlet) 
(require web-server/servlet-env)
(require json)

(define (http-response content)  
  (response/full
    200                  ; HTTP response code.
    #"OK"                ; HTTP response message.
    (current-seconds)    ; Timestamp.
    TEXT/HTML-MIME-TYPE  ; MIME type for content.
    '()                  ; Additional HTTP headers.
    (list                ; Content (in bytes) to send to the browser.
      (string->bytes/utf-8 content))))

(define (show-time-page request)
  (http-response (number->string (current-seconds))))


(define (greeting-page request)  
  (http-response (list-ref '("Hi" "Hello") (random 2))))

; this is the example-post ...)
(define (example-post request)
  (define rtn (format "{ \"ok\" : \"message ok\" }"))
  (define data (bytes->string/utf-8 (request-post-data/raw request)))
  (let* ([hsh (string->jsexpr data)]
        [name (hash-ref hsh 'name)]
        [age (hash-ref hsh 'age)]
        [hobbies (hash-ref hsh 'hobbies)])
    (displayln (format "~v: ~v -> ~v~%" name age hobbies))
    (http-response rtn)))


;; URL routing table (URL dispatcher).
(define-values (dispatch generate-url)
  (dispatch-rules
    [("time") show-time-page]
    [("hello") greeting-page]  ; Notice this line.
    [("example-post")  #:method "post" example-post]
    [else (error "There is no procedure to handle the url.")]))



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