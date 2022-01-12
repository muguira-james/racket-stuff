#lang racket
(require 2htdp/universe 2htdp/image)

;
; define some constants
(define WIDTH 600)
(define HEIGHT 400)

;
; bring the image in from disk and scale / size it
(define crane (scale/xy 0.25 0.25 (bitmap "flying-crane.png")))

;
; my WorldState
(struct bird-posn (x y))

;
; my drawing function
(define (render w)
  (place-image crane (bird-posn-x w) (bird-posn-y w) (empty-scene WIDTH HEIGHT)))

;
; advance the bird  along the y axis
(define (handle-tick w)
  (bird-posn (+ 10 (bird-posn-x w)) (bird-posn-y w)))

;
; get key board keys
(define (handle-key w key)
  (cond
    [(key=? key "q") (stop-with w)]
    [else w]))

;
; start the simulation
(define (start)
  (big-bang
    (bird-posn 80 80)
    (to-draw render)
    (on-tick handle-tick)
    (on-key handle-key)))
  