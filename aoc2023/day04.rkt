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
  (match (regexp-match #rx"^Card[ ]+([0-9]+):([0-9 ]+)[|]([0-9 ]+)$" s)
    [(list _ cardno win have)
     (card (parse-nums win) (parse-nums have))]))

;; parse-nums : String -> (Listof Integer)
(define (parse-nums s)
  (map string->number (string-split (string-trim s))))

;; card-nwins : Card -> Nat
(define (card-nwins c)
  (match-define (card win have) c)
  (length (filter (lambda (n) (member n win)) have)))

;; card-value : Card -> Nat
(define (card-value c)
  (define nwins (card-nwins c))
  (if (zero? nwins) 0 (expt 2 (sub1 nwins))))

(module+ test
  (require rackunit)
  (check-equal? (parse-line (list-ref example 0))
                (card '(41 48 83 86 17) '(83 86 6 31 17 9 48 53)))
  (check-equal? (card-value (parse-line (list-ref example 0)))
                8))

;; ----------------------------------------


;; W(n) = number of winning numbers on card n
;; T(n) = number of total cards arising from 1 copy of card n (including original)
;; T(n) = 1 + sum(i = 1..W(n)) T(n+i)

;; part2 : (Vectorof Nat) -> ??
(define (part2 W-vec)
  (define T-vec (make-vector (vector-length W-vec) #f))

  ;; W : Nat -> Nat
  (define (W n) (vector-ref W-vec (sub1 n)))

  ;; T : Nat -> Nat
  (define (T n)
    (or (vector-ref T-vec (sub1 n))
        (let ([result (compute-T n)])
          (vector-set! T-vec (sub1 n) result)
          result)))
  (define (compute-T n)
    (+ 1 (for/sum ([i (in-range 1 (add1 (W n)))]) (T (+ n i)))))

  ;; sum for all original cards
  (for/sum ([i (in-range 1 (add1 (vector-length W-vec)))]) (T i)))

(module+ test
  (define cards (map parse-line example))
  (check-equal? (part2 (list->vector (map card-nwins cards)))
                30))

(module+ main
  (define lines (port->lines))
  (define cards (map parse-line lines))

  ;; part 1
  (for/sum ([c cards]) (card-value c))

  ;; part 2
  (define v (list->vector (map card-nwins cards)))
  (part2 v))
