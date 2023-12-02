#lang racket

;; An RGB is (list Nat Nat Nat),
;; representing number of red, green, and blue cubes, respectively.

;; rgb000 : RGB
(define rgb000 (list 0 0 0))

;; rgb+ : RBG RBG -> RGB
(define (rgb+ x y)
  (match* [x y]
    [[(list xr xg xb) (list yr yg yb)]
     (list (+ xr yr) (+ xg yg) (+ xb yb))]))

;; rgbmax : RBG RBG -> RGB
(define (rgbmax x y)
  (match* [x y]
    [[(list xr xg xb) (list yr yg yb)]
     (list (max xr yr) (max xg yg) (max xb yb))]))

;; A Game is (game Nat (Listof RGB))
(struct game (index hands) #:prefab)

;; parse-game : String -> Game
(define (parse-game line)
  (match (regexp-match "^Game ([0-9]+): (.*)$" line)
    [(list _ index hands)
     (game (string->number index)
           (map parse-hand (regexp-split #rx";[ ]*" hands)))]))

;; parse-hand : String -> RGB
(define (parse-hand s)
  (define parts (regexp-split #rx",[ ]*" s))
  (foldr rgb+ rgb000 (map parse-rgb parts)))

;; parse-rgb : String -> RGB
(define (parse-rgb s)
  (match (regexp-match #rx"^([0-9]+) (red|blue|green)$" s)
    [(list _ number color)
     (define n (string->number number))
     (match color
       ["red" (list n 0 0)]
       ["green" (list 0 n 0)]
       ["blue" (list 0 0 n)])]))

(module+ test
  (require rackunit)

  (check-equal? (parse-rgb "4 red")
                (list 4 0 0))
  (check-equal? (parse-hand "4 red, 1 blue")
                (list 4 0 1))

  (check-equal?
   (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")
   (game 1 '((4 0 3) (1 2 6) (0 2 0))))
  (check-equal?
   (parse-game "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue")
   (game 2 '((0 2 1) (1 3 4) (0 1 1))))
  (check-equal?
   (parse-game "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red")
   (game 3 '((20 8 6) (4 13 5) (1 5 0))))
  (check-equal?
   (parse-game "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
   (game 4 '((3 1 6) (6 3 0) (14 3 15))))
  (check-equal?
   (parse-game "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
   (game 5 '((6 3 1) (1 2 2)))))

;; ----

;; make-game-possible : Nat*3 -> Game -> Boolean
(define (make-game-possible? nred ngreen nblue)
  (define hand-ok? (make-hand-possible? nred ngreen nblue))
  (lambda (g) (match g [(game index hands) (andmap hand-ok? hands)])))

;; make-hand-possible : Nat*3 -> RGB -> Boolean
(define ((make-hand-possible? nred ngreen nblue) hand)
  (match hand
    [(list hred hgreen hblue)
     (and (<= 0 hred nred)
          (<= 0 hgreen ngreen)
          (<= 0 hblue nblue))]))

;; game-ok? : Game -> Boolean
;; only 12 red cubes, 13 green cubes, and 14 blue cubes
(define game-ok? (make-game-possible? 12 13 14))

;; ----

;; game-needed-rgb : Game -> RGB
;; returns the minimum RGB needed for all hands to be viable
(define (game-needed-rgb g)
  (match g [(game index hands) (foldr rgbmax rgb000 hands)]))

;; rgb-power : RGB -> Nat
(define (rgb-power hand)
  (match hand [(list r g b) (* r g b)]))

;; ----

(module+ main
  (define games (for/list ([line (in-lines)]) (parse-game line)))

  (for/sum ([g (in-list games)] #:when (game-ok? g))
    (game-index g))

  (for/sum ([g (in-list games)])
    (rgb-power (game-needed-rgb g))))
