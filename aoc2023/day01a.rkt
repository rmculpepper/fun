#lang racket/base
(require racket/match
         racket/port)

;; line-value : String -> Integer/NaN
(define (line-value line)
  (define a (regexp-match #rx"^[^0-9]*([0-9])" line))
  (define z (regexp-match #rx"([0-9])[^0-9]*$" line))
  (cond [(and a z) (string->number (string-append (cadr a) (cadr z)))]
        [else (eprintf "failed: ~s\n" line) +nan.0]))

(module+ test
  (require rackunit)

  (check-equal? (line-value "1abc2")
                12)
  (check-equal? (line-value "pqr3stu8vwx")
                38)
  (check-equal? (line-value "a1b2c3d4e5f")
                15)
  (check-equal? (line-value "treb7uchet")
                77))

(module+ main
  (for/sum ([line (in-lines)])
    (line-value line)))
