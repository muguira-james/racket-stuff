;;
;; Simulation
;;
;; simulate fire propagation along thunder ridge.
;;
;; This  uses Racket's world simulation structures to handle drawing, & key board input.
;;
#lang racket
; james
(require 2htdp/universe 2htdp/image)
(require lang/posn)

;
; constants for drawing
(define WIDTH 420)
(define HEIGHT 150)
(define LAST-X 430)

;
; bring in the tree images from disk
(define pine-img (scale/xy 0.0125 0.0125 (bitmap "pinetree.png")))
(define pinefire-img (scale/xy 0.125 0.125 (bitmap "pinefire.png")))
(define no-tree-img (scale/xy 0.125 0.25 (bitmap "meadow.png")))

;
; the node that holds our tree info
; note:
;   flag is either 0=no-tree ; 1=pine-tree; 2=tree on fire
;   posn is a structure of (x y)
;   #transparent just make the struct print nicely
(struct tree-struct (flag posn) #:transparent)

; struct for handling the world
;
; t-ridge is a structure with a "ridge collection" and a starting season
; the ridge-collection is 10 tree images.
;
; a tree image is 40 pixels wide
; so, to be neat on the screen, we start at x=pixel 30 and every 40 pixels,
;   we drop either a meadow or tree
;
; seasons are:
; 0 = spring, make meadows
; 1 = summer, grow trees
; 2 = fall, fire season
;
(struct t-ridge (ridge season) #:transparent)

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
      

;
; takes a collection and renders on an empty scene
(define (show-ridge collection)
  ; (displayln  collection)
  (place-images (get-scene-images collection) (get-scene-positions collection) (empty-scene WIDTH HEIGHT)))


; 
; creates a single tree struct with a tree type, an x position and a y position
(define (make-a-tree flag x y)
  (tree-struct flag (make-posn x y)))

; create a tree using a position (posn)
(define (make-a-tree/p flag posn)
  (make-a-tree flag (posn-x posn) (posn-y posn)))

;
; does this square contain a tree?
(define (contains-tree? t-struct)
  (if (= 1 (tree-struct-flag t-struct))
      #t
      #f))

(define (clear-burnt-trees t-struct)
  (if (= 2 (tree-struct-flag t-struct))
      (make-a-tree/p 0 (tree-struct-posn t-struct))
      t-struct))
;
; decide if we  put a tree here, or burn the tree that is here
(define (lighting? t-struct)
  (let ((flag (random 2)))
    (if (contains-tree? t-struct)
        (if  (= 1 flag)
             ; return a t-struct: flag=fire, posn=(x, y)
             (make-a-tree/p 2 (tree-struct-posn t-struct))
             t-struct)
        t-struct)))
    
      
      
; not a tree here now?, decide if I should put a tree here
(define (grow-a-tree? t-struct)
  (let ((flag (random 2)))
        (if (= 1 flag)
            (make-a-tree/p flag (tree-struct-posn t-struct))
            t-struct)))
;
; loop over the ridge call the input function, f
(define (check-tree-agents? f collection)
  (for/list ((i collection))
             (f i)))

;
; using the last x position, return a list of (possible) trees
; since the x position counter starts at 30, to make 10 spots, n=430
;
; this counts out 10 tree positions,
;  the 1st is at (0,90), the last is at (390, 90),
;  a tree image is about 40 pixels wide
;
; lAST-X controls how many pixels along the x axis to go
(define (populate-ridge)
  (for/list ([x (range 30 LAST-X 40)])
                  (make-a-tree 0 x 90)))


;;;
;;;
;;  the rest here  is all world handlers
;;;
;; Big-bang uses a "world" struct. It hands this struct to each expression defined in the sim.
;; Each  of the expressions MUST return a complete world struct

;
; return a new ridge, populated with trees. This runs on the tick.
; It might grow trees and it always returns a complete world struct
(define (tickr w)
  (define ridge (t-ridge-ridge w))
  (define season (t-ridge-season w))
  (cond
    ((= 0 season) (t-ridge (check-tree-agents? grow-a-tree? ridge) 1 ))
    ((= 1 season) (t-ridge (check-tree-agents? lighting? ridge) 2))
    (else
     (t-ridge (check-tree-agents? clear-burnt-trees ridge) 0))))
    
;
; handle key presses
(define (deal-with-guess w key)
  (cond 
        [(key=? key "q") (stop-with w)]
        [else w]))
        
;
; draw the ridge scene
; this does not have to return a world struct
(define (render w)
  ; (displayln w)
  (show-ridge (t-ridge-ridge w)))
  
;
; set up and run the simulation with "world", w
; notice how it starts with a complete  world: (t-ridge (list of spaces on the ridge that might have trees))
;
; so, the thunder ridge world (w) is:
;  "t-ridge" - 10 spaces for trees (each being about 40 pixels wide) and
;  the season is spring (0)
(define  (start )
  (displayln "press 'q' to stop")
  (big-bang (t-ridge (populate-ridge) 0)
    (to-draw render)
    (on-key deal-with-guess)
    (on-tick tickr 2)))

;;
;; explore how to create a "find contiguous sequence"

; (define l (list 0 1 0 1 1 2 1 0 1 1))
; (define l (list 1 1 2 0 0 1 1 0 0 1))
; (define l (list 1 1 2 0 1 0 1 0 1 0))
(define l (list 0 1 0 2 0 1  0 1 1 0))
; (define vv (list->vector l))
      
    
(define (za l)
  (define cnt 0)
  (define start -1)
  (define lentrees 0)

  (define (iter acc l)
    ;(displayln (format "--> len=~a lis=~a" (length l) l)) 
    
    (cond
      ; end of list found
      [(= 0 (length l))
       (if (= 0 lentrees)
            (displayln "")
            (displayln (format "_e_ start=~a acc=~a: len=~a" start acc lentrees )))]
      ; not in a string of trees
      [(and (= 0 (first l)) (= 0 lentrees)
            (begin              
              (set! lentrees 0)
              ; (displayln (format "_n_ start=~a acc=~a: len=~a -- ch=~a -> l=~a" start acc lentrees (first l) l ))
              (iter (+ 1 acc) (rest l))))]
     
      ; found end of string of trees
      [(and (= 0 (first l)) (> lentrees 0)
            (begin                
              (displayln (format "_e_ start=~a; acc=~a: len=~a -- ch=~a -> l=~a" start acc lentrees (first l) l ))
              (set! lentrees 0)
              (set! start -1)
              (iter (+ 1 acc) (rest l))))]

      ; start a string of trees
      [else
       (begin           
         (set! lentrees (+ lentrees 1))
         (if (= start -1)
             (set! start acc)
             #t)
         ; (displayln (format "_i_start=~a; acc=~a: len=~a -- ch=~a -> l=~a" start acc lentrees (first l) l ))
         (iter (+ 1 acc) (rest l)))]))
(iter 0 l))
 