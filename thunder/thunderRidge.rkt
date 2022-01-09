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
(struct tree-struct (flag posn) #:transparent)

; struct for handling the world
(struct t-ridge (ridge) #:transparent)

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

;
; decide if we  put a tree here
(define (whip-up-tree? t-struct)
  (let ((flag (random 2)))
    (if (= 1 flag)
        (make-a-tree/p flag (tree-struct-posn t-struct))
        t-struct)))

;
; loop over the ridge call the input function, f
(define (grow-trees? f collection)
  (for/list ((i collection))
             (f i)))
  

;
; randomly create a tree in a given position - not used
(define (create-tree/posn collection)
  (cond [(empty? collection) empty]
        [else
         [cons (make-a-tree/p (random 2) (tree-struct-posn (first collection)))
               (create-tree/posn (rest collection))]]))

;
; using the last x position, return a list of (possible) trees
; since the x position counter starts at 30, to make 10 spots, n=430
(define (make-ridge)
  (for/list ([x (range 30 LAST-X 40)])
                  (make-a-tree 0 x 90)))


;;;
;;;
;;  the rest here  is a ll world handlers
;;;
;; Big-bang uses a "world" struct. It hands this struct to each expression defined in the sim.
;; Each  of the expressions MUST return a complete world struct

;
; return a new ridge, populated with trees. This runs on the tick.
; It might grow trees and it always returns a complete world struct
(define (tickr w)
    (define ridge (t-ridge-ridge w))
    (t-ridge (grow-trees? whip-up-tree? ridge)))
    
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
(define  (start )
  (big-bang (t-ridge (make-ridge))
    (to-draw render)
    (on-key deal-with-guess)
    (on-tick tickr 2)))

