#lang racket/base
(require racket/match
         racket/list
         racket/port)

;; token-words : (Listof String)
;; Value of string is index in list.
(define token-words
  '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

;; token-rx : Regexp
(define token-rx
  #rx"[0-9]|zero|one|two|three|four|five|six|seven|eight|nine")

;; token-value : String -> Nat
(define (token-value w)
  (or (index-of token-words w)
      (string->number w)))

;; tokenize : String [Nat] -> (Listof String)
;; Discard non-token chars, catch overlaps (see tests)
(define (tokenize s [start 0])
  (match (regexp-match-positions token-rx s start)
    [(list (cons na nz))
     ;; Resume starting at na+1 to catch overlaps.
     (cons (substring s na nz) (tokenize s (add1 na)))]
    [#f null]))

;; line-value : String -> Nat
(define (line-value line)
  (define tokens (tokenize line))
  (+ (* 10 (token-value (first tokens)))
     (token-value (last tokens))))

(module+ test
  (require rackunit)

  (check-equal? (line-value "1abc2") 12)
  (check-equal? (line-value "pqr3stu8vwx") 38)
  (check-equal? (line-value "a1b2c3d4e5f") 15)
  (check-equal? (line-value "treb7uchet") 77)

  (check-equal? (line-value "two1nine") 29)
  (check-equal? (line-value "eightwothree") 83)
  (check-equal? (line-value "abcone2threexyz") 13)
  (check-equal? (line-value "xtwone3four") 24)
  (check-equal? (line-value "4nineeightseven2") 42)
  (check-equal? (line-value "zoneight234") 14)
  (check-equal? (line-value "7pqrstsixteen") 76)

  ;; overlaps
  (check-equal? (tokenize "eightwothree") '("eight" "two" "three"))
  (check-equal? (tokenize "twone") '("two" "one"))
  (void))

(module+ main
  (for/sum ([line (in-lines)])
    (line-value line)))
