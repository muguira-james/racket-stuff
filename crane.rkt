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
; just return the bird position struct
(define (handle-tick w)
  (let ((x-pos (bird-posn-x w))
        (y-pos (bird-posn-y w)))
  (bird-posn (bird-posn-x w) (bird-posn-y w))))

;
; change the bird position struct by moving the bird down 10 pixels
(define (handle-down w)
  (bird-posn (bird-posn-x w) (+ 10 (bird-posn-y w))))

;
; change bird position by moving bird up 10 pixels
(define (handle-up w)
  (bird-posn (bird-posn-x w) (- (bird-posn-y w) 10)))

;
; change bird position, moving it left 10 pixels
(define (handle-left w)
  (bird-posn (- (bird-posn-x w) 10) (bird-posn-y w)))

;
; change bird position, moving it right 10 pixels
(define (handle-right w)
  (bird-posn (+ (bird-posn-x w) 10) (bird-posn-y w)))
;
; get key board keys
(define (handle-key w key)
  (cond
    [(key=? key "q") (stop-with w)]
    [(key=? key "down") (handle-down w)]
    [(key=? key "up")   (handle-up w)]
    [(key=? key "left") (handle-left w)]
    [(key=? key "right") (handle-right w)]
    [else w]))

;
; start the simulation
(define (start)
  (big-bang
    (bird-posn 80 80)
    (to-draw render)
    (on-tick handle-tick)
    (on-key handle-key)))
  