#lang racket

(require rackunit)

(require web-server/servlet) 
(require web-server/servlet-env)
(require json)
(require data/queue)

(require "middleware.rkt")


(test-case "middleware: contains-topic"
           ; setup by adding a topic 'key and an element 'aaa
           (add-data-to-topic 'key 'aaa)
           (check-equal? (contains-topic 'key) #t "should contain the test topic")
           (check-equal? (contains-topic 'foo) #f "should not contain 'foo' topic")
           (check-equal? (hash-count topic-hash) 1 "should only be a single topic")
           ; clean up before next test
           (hash-remove! topic-hash 'key)
           )

(test-case "middleware: add-data-to-topic"
           ; first, should be nothing in the topic hash
           (check-equal? (hash-count  topic-hash) 0 "should be empty")
           ; add a single topic and 1 item
           (add-data-to-topic 'key 'aaa)
           (check-equal? (hash-count topic-hash) 1 "should only be a single topic")
           (check-equal? (queue-length (hash-ref topic-hash 'key)) 1 "should only be a  single item in the queue")
           ; add a 2nd item to the test topic
           (add-data-to-topic 'key 'bbb)
           (check-equal? (hash-count topic-hash) 1 "should be a single topic")
           (check-equal? (queue-length (hash-ref topic-hash 'key)) 2 "should only be a  single item in the queue")

           (add-data-to-topic 'key2 'zzz)
           (check-equal? (hash-count topic-hash) 2 "should be 2 topics now")
           (check-equal? (queue-length (hash-ref topic-hash 'key)) 2 "should only be 2 items in the 'key queue")
           (check-equal? (queue-length (hash-ref topic-hash 'key2)) 1 "should only be 1 item in the 'key2 queue")

           (add-data-to-topic 'key2 "go man go")
           (check-equal? (queue-length (hash-ref topic-hash 'key2)) 2 "should be 2 items in the 'key2 queue")

           (add-data-to-topic 'key2 "[ 'foo 'bar ]")
           (check-equal? (queue-length (hash-ref topic-hash 'key2)) 3 "should be 2 items in the 'key2 queue")
           (check-equal? (dequeue! (hash-ref topic-hash 'key2)) 'zzz "should be the first string zzz")

           
           (hash-remove! topic-hash 'key)
           (hash-remove! topic-hash 'key2)
           )

(test-case "middleware: remove-data-from-topic"
           ; first, should be nothing in the topic hash
           (check-equal? (hash-count  topic-hash) 0 "should be empty")
           ; add a single topic and 1 item
           (add-data-to-topic 'key 'aaa)
           (check-equal? (hash-count topic-hash) 1 "should only be a single topic")
           (check-equal? (queue-length (hash-ref topic-hash 'key)) 1 "should only be a  single item in the queue")
           ; ... now remove stuff

           (check-equal? (remove-data-from-topic 'key) "{ \"topic-name\": key \"payload\": 'aaa }" "remove only item in the queue")
           ;(check-equal? (remove-data-from-topic 'key)  "should be nothing in the queue")
           (check-equal? (remove-data-from-topic 'key) "{ \"error\": \"no data in queue 'key\" }\n" "should get an error return")

           ; clean up
           (hash-remove! topic-hash 'key)
           )

(test-case "middleware: get-queue-for-topic"
           ;check to make sure things are clear
           (check-equal? (hash-count  topic-hash) 0 "should be empty")

           ; 1st what happens with nothing the hash or queue?
           (check-equal? (queue? (get-queue-for-topic 'key)) #t "empty queue")
           (check-equal? (queue-empty? (get-queue-for-topic 'key)) #t "empty queue")

           ; now add something to the queue
           (add-data-to-topic 'key 'aaa)
           (check-equal? (queue? (get-queue-for-topic 'key)) #t "empty queue")
           (check-equal? (queue-empty? (get-queue-for-topic 'key)) #f "empty queue")

           ; clean up
           (hash-remove! topic-hash 'key)
           )