
#lang racket
; james
(require 2htdp/universe 2htdp/image)
(require lang/posn)

(define WIDTH 420)
(define HEIGHT 150)
(define last-x 430)

;
; bring in the tree images from disk
(define pine-img (scale/xy 0.0125 0.0125 (bitmap "pinetree.png")))
(define pinefire-img (scale/xy 0.125 0.125 (bitmap "pinefire.png")))
(define no-tree-img (scale/xy 0.125 0.25 (bitmap "meadow.png")))

;
; the node that holds our tree info
(struct tree-struct (flag posn) #:transparent)
(struct t-ridge (ridge) #:transparent)

;
; takes a field, full of place-for-tree structures, and draws them in a empty scene
(define (show-ridge collection)
  ; (displayln  collection)
  (place-images (get-scene-images collection) (get-scene-positions collection) (empty-scene WIDTH HEIGHT)))


; 
; creates a single tree struct with a tree type, an x position and a y position
(define (make-a-tree flag x y)
  (tree-struct flag (make-posn x y)))

(define (make-a-tree/p flag posn)
  (make-a-tree flag (posn-x posn) (posn-y posn)))

(define (contains-tree? t-struct)
  (if (= 1 (tree-struct-flag t-struct))
      #t
      #f))

(define (whip-up-tree? t-struct)
  (let ((flag (random 2)))
    (if (= 1 flag)
        (make-a-tree/p flag (tree-struct-posn t-struct))
        t-struct)))

(define (grow-trees? f collection)
  (for/list ((i collection))
             (f i)))
  

;
; randomly create a tree in a given position
(define (create-tree/posn collection)
  (cond [(empty? collection) empty]
        [else
         [cons (make-a-tree/p (random 2) (tree-struct-posn (first collection)))
               (create-tree/posn (rest collection))]]))

;
; using the last x position, return a list of (possible) trees
; since the x position counter starts at 30, to make 10 spots, n=430
(define (make-ridge)
  (for/list ([x (range 30 last-x 40)])
                  (make-a-tree 0 x 90)))

; (place-images (list pine pinefire) (list (make-posn 30 90) (make-posn 150 90)) (empty-scene WIDTH HEIGHT))
;(define field (list
;               (make-a-tree/p 0 (make-posn 30 90))
;               (make-a-tree/p 1 (make-posn 70 90))
;               (make-a-tree/p 1 (make-posn 110 90))
;               (make-a-tree/p 2 (make-posn 150 90))))

;
; convert a flag to a image
(define (image-for-flag flag)
  (cond [(= flag 0) no-tree-img]
        [(= flag 1) pine-img]
        [(= flag 2) pinefire-img]
        [else empty]))

;
; return the images found in the field collection
(define (get-scene-images collection)
  (cond [(empty? collection) empty]
        [else
         (cons (image-for-flag (tree-struct-flag (first collection)))
               (get-scene-images (rest collection)))]))

;
; return the positions found in the field collection
(define (get-scene-positions collection)
  (cond [(empty? collection) empty]
      [else
       (cons (tree-struct-posn (first collection))
             (get-scene-positions (rest collection)))]))
      

;;;
;;;
;;  the rest here  is a ll world handlers
(define (tickr w)
    (define ridge (t-ridge-ridge w))
    (t-ridge (grow-trees? whip-up-tree? ridge)))
;
;
(define (deal-with-guess w key)
  (cond 
        [(key=? key "q") (stop-with w)]
        [else w]))
;
;
(define (render w)
  ; (displayln w)
  (show-ridge (t-ridge-ridge w)))
;
(define  (start )
  (big-bang (t-ridge (make-ridge))
    (to-draw render)
    (on-key deal-with-guess)
    (on-tick tickr 2)))

;; (define field (make-field 430))