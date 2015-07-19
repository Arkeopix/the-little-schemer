#lang racket
(require 2htdp/universe 2htdp/image)


(struct game-state (small big try) #:transparent)
(define TEXT-SIZE 14)
(define HELP-TEXT
  (text "↑ for larger number, ↓ for smaller ones"
        TEXT-SIZE
        "blue"))
(define HELP-TEXT2
  (text "Press = when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 250)
(define COLOR "red")
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 205)
(define SIZE 75)
(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))

(define (smaller w)
  (game-state (game-state-small w)
              (+ game-state-try 1)
              (max (game-state-small w) (sub1 (guess w)))))

(define (bigger w)
  (game-state (min (game-state-big w) (add1 (guess w)))
              (+ game-state-try 1)
              (game-state-big w)))

(define (guess w)
  (quotient (+ (game-state-small w) (game-state-big w)) 2))

(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC) (game-state (game-state-try w)))

(define (render-last-scene w)
  (overlay (text "End" SIZE COLOR) MT-SC))

(define (single? w)
  (= (game-state-small w) (game-state-big w)))

(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

(define (start lower upper)
  (big-bang (game-state lower upper 0)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

