#lang racket

(define (eval-line line)
  (match (regexp-match #rx"mul[(]([0-9]+),([0-9]+)[)](.*)$" line)
    [#f 0]
    [(list _ (app string->number a) (app string->number b) rest)
     (+ (* a b) (eval-line rest))]))

(define (eval-line2 on? line)
  (let loop ([on? on?] [acc 0] [line line])
    (match (regexp-match #rx"(mul[(]([0-9]+),([0-9]+)[)]|do[(][)]|don't[(][)])(.*)$" line)
      [(list _ "do()" _ _ rest)
       (loop #t acc rest)]
      [(list _ "don't()" _ _ rest)
       (loop #f acc rest)]
      [(list _ _ (app string->number a) (app string->number b) rest)
       (loop on? (+ (if on? (* a b) 0) acc) rest)]
      [#f (values on? acc)])))

(printf "Part 1:\n")
(define lines (sequence->list (in-lines)))
(for/sum ([line (in-list lines)]) (eval-line line))

(printf "Part 2:\n")
(for/fold ([on? #t] [acc 0] #:result acc)
          ([line (in-list lines)])
  (define-values (next-on? n) (eval-line2 on? line))
  (values next-on? (+ acc n)))
