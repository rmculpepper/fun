#lang racket
(provide (all-defined-out))

;; hand-fourier : String -> (values Nat (Listof (cons Char Nat)))
(define (hand-fourier hand)
  (define counth
    (for/fold ([h (hash)]) ([c (in-string hand)])
      (hash-update h c add1 0)))
  (values (hash-ref counth #\J 0)
          (sort (hash-map (hash-remove counth #\J) cons) > #:key cdr)))

;; hand-type : Hand -> Nat, for comparison (higher is better)
(define (hand-type hand)
  (define-values (jokers cfreqs) (hand-fourier hand))
  ;; You're always better off making Jokers count as most frequent card.
  (define freqs*
    (match (map cdr cfreqs)
      [(cons hi-freq freqs) (cons (+ jokers hi-freq) freqs)]
      ['() (list jokers)]))
  (match freqs*
    [(list 5) 6]
    [(list 4 1) 5]
    [(list 3 2) 4]
    [(list 3 1 1) 3]
    [(list 2 2 1) 2]
    [(list 2 1 1 1) 1]
    [(list 1 1 1 1 1) 0]))

(define card-order (string->list "J23456789TJQKA"))
(define card-replace             "abcdefghijklmn")

;; hand-to-cmpable : Hand -> String
(define (hand-to-cmpable hand)
  (list->string
   (for/list ([c (in-string hand)])
     (string-ref card-replace (index-of card-order c)))))

;; hand2<? : Hand Hand -> Boolean
(define (hand2<? h1 h2)
  (string<? (hand-to-cmpable h1) (hand-to-cmpable h2)))

;; hand<? : Hand Hand -> Boolean
(define (hand<? h1 h2)
  (define t1 (hand-type h1))
  (define t2 (hand-type h2))
  (cond [(= t1 t2) (hand2<? h1 h2)]
        [else (< t1 t2)]))

(define (hand>? h1 h2) (hand<? h2 h1))

;; ----

(define (parse-line line)
  (match (regexp-match #rx"^([0-9TJQKA]+)[ ]*([0-9]+)$" line)
    [(list _ hand bid-s)
     (cons hand (string->number bid-s))]))

(define (process rounds)
  (define sorted (sort rounds hand<? #:key car))
  (for/sum ([round (in-list sorted)] [index (in-naturals 1)])
    (* index (cdr round))))

(module+ test
  (require rackunit)

  (define lines
    '("32T3K 765"
      "T55J5 684"
      "KK677 28"
      "KTJJT 220"
      "QQQJA 483"))

  (define rounds (map parse-line lines))
  ;; (map hand-type (map car rounds))

  (check-equal? (process rounds) 6440))

(module+ main
  (define lines (port->lines))
  (define rounds (map parse-line lines))
  (process rounds))
