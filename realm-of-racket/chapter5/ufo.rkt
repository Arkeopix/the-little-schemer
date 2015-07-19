#lang racket
(require 2htdp/universe 2htdp/image)

(define WIDTH 400)
(define HEIGHT 300)
(define IMAGE-OF-UFO (bitmap/file "assets/rsz_2penguin_ufo.png"))

(define (add-3-to-state current-state)
  (+ current-state 3))

(define (draw-a-ufo-onto-empty-scene current-state)
  (place-image IMAGE-OF-UFO (/ WIDTH 2) current-state
               (empty-scene WIDTH HEIGHT)))

(define (state-is-300 current-state)
  (>= current-state 300))

(big-bang 0
          (on-tick add-3-to-state)
          (to-draw draw-a-ufo-onto-empty-scene)
          (stop-when state-is-300))

