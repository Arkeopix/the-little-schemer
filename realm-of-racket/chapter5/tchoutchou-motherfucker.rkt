#lang racket
(require 2htdp/universe 2htdp/image)

(define WIDTH 400)
(define HEIGHT 200)
(define TRAIN-IMAGE (bitmap/file "assets/train.png"))

(define (sub-3-to-state state)
  (cond [(> state 0) (- state 3)]
        [else (+ state WIDTH)]))

(define (draw-train state)
  (place-image TRAIN-IMAGE state (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

(big-bang WIDTH
          (on-tick sub-3-to-state)
          (to-draw draw-train))
