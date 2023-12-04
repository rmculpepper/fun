#lang racket

;; ----------------------------------------

(define example
  '("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(struct card (win have) #:prefab)

;; parse-line : String -> Card
(define (parse-line s)
  (match (regexp-match #rx"^Card ([0-9]+):([0-9 ]+)[|]([0-9 ]+)$" s)
    [(list _ cardno win have)
     (card (parse-nums win) (parse-nums have))]))

;; parse-nums : String -> (Listof Integer)
(define (parse-nums s)
  (map string->number (string-split (string-trim s))))

;; card-value : Card -> Nat
(define (card-value c)
  (match-define (card win have) c)
  (define have-win (filter (lambda (n) (member n win)) have))
  (if (pair? have-win) (expt 2 (sub1 (length have-win))) 0))

(module+ test
  (require rackunit)
  (check-equal? (parse-line (list-ref example 0))
                (card '(41 48 83 86 17) '(83 86 6 31 17 9 48 53)))
  (check-equal? (card-value (parse-line (list-ref example 0)))
                8))

(module+ main
  (define lines (port->lines))
  (define cards (map parse-line lines))
  (for/sum ([c cards]) (card-value c)))
