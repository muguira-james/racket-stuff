#lang racket
(require 2htdp/universe 2htdp/image)

(define WIDTH 600)
(define HEIGHT 400)

(define crane (scale/xy 0.25 0.25 (bitmap "flying-crane.png")))

(struct bird-posn (x y))

(define (render w)
  (place-image crane (bird-posn-x w) (bird-posn-y w) (empty-scene WIDTH HEIGHT)))

(define (handle-tick w)
  (bird-posn (+ 10 (bird-posn-x w)) (bird-posn-y w)))

(define (handle-key w key)
  (cond
    [(key=? key "q") (stop-with w)]
    [else w]))

(define (start)
  (big-bang
    (bird-posn 80 80)
    (to-draw render)
    (on-tick handle-tick)
    (on-key handle-key)))
  