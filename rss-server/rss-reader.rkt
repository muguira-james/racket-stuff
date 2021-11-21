#lang racket
;
; a server to get rss news from different sites.
;
; not very clever yet.  Each site has specific parsing to extract the set { title, link, description }
;
; has limitation that we have to write the rss data to a file and then read it back in.
(require net/http-easy)
(require xml
         xml/path)

(require simple-xml)

(define feed-table (make-hash))

;
; the feed table is just a hash of different websites we get rss from
(hash-set! feed-table 'cnn "http://rss.cnn.com/rss/cnn_us.rss")
(hash-set! feed-table 'bbc "http://feeds.bbci.co.uk/news/world/rss.xml")
(hash-set! feed-table 'slashdot "http://rss.slashdot.org/Slashdot/slashdotMain")

; (define res (get (hash-ref feed-table 'cnn )))

(define (get-response feed-symbol)
  (get (hash-ref feed-table feed-symbol)))

(define (get-xexpr-from-feed-symbol feed-symbol)
  (response-xexpr (get-response  feed-symbol)))

;
; look up  the symbol in the feed-table, use that url to get xml and write it to a file called out.xml
(define (get-write-symbol symbol)
  (let ([out (open-output-file "out.xml" #:exists 'replace)])
        (write-xexpr (get-xexpr-from-feed-symbol symbol) out)
        (close-output-port out)))




(define hsh (make-hash))


;
; walk the list and save "items" in a hash
;
; but what I really want to do is save 'title, 'link, 'description
; for EACH story in the rss feed
(define  (my-walker lst1 mhash)
  ; take a key and value and inject it into the external hash called: hsh
  ; the key is specific to cnn. It is of the form: rss.channel.item + some number
  ; the val is the coorsponding title for that item
  (define (inject key val)
    (cond
      ; check if the key is unique and add the { key, val{title}} to the hash
      [(not (hash-has-key? hsh key))
       (displayln (format "~s ~s" key (hash-ref mhash key)))
       (hash-set! hsh key (hash-ref mhash key))]))
  ;
  ; iterate through the list of keys from the rss-hash (that was generated from out.xml)
  (define (iter lst num)
    (cond
      [(empty? lst) num]
      [else
       (cond
         ; if the thing is a itemDD
         [(regexp-match? #px"item\\d+\\.title$"  (first lst))
               ; inject it into the hsh
           (inject (first lst) (first lst) )])
       (iter (rest lst) (+ num 1))]))
  (iter lst1 0))




; (define rss-hash (xml->hash "./out.xml"))

(define (get-cnn-rss)
  (begin
    ; 1st create a .out.xml file with the rss from the news site in the local file system
    (get-write-symbol 'cnn)
    ; read that .out.xml file back in as a hash called rss-hash
    (define rss-hash (xml->hash "./out.xml"))
    ; walk that rss-hash to create the rss items I want to read??
    (my-walker (hash-keys rss-hash) rss-hash)))
