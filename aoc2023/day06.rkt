#lang racket

;; parse-data : -> (Listof (list Nat Nat))
(define (parse-data)
  (map list
       (parse-labeled-ints (read-line))
       (parse-labeled-ints (read-line))))

;; parse-labeled-ints : String -> (Listof Nat)
(define (parse-labeled-ints line)
  (match (regexp-match "^[a-zA-Z]*:[ ]*([0-9 ]+)" line)
    [(list _ nums)
     #;(map string->number (string-split (string-trim nums)))
     (list (string->number (apply string-append (string-split (string-trim nums)))))]))

;; distance : Nat Nat -> Nat
(define (distance hold-time total-time)
  (max 0 (* hold-time (- total-time hold-time))))

(define (race-ways-to-beat t d)
  (for/sum ([hold-time (in-range 1 t)]
            #:when (> (distance hold-time t) d))
    1))

(define (total-ways races)
  (for/product ([race (in-list races)])
    (race-ways-to-beat (car race) (cadr race))))

(module+ test
  (require rackunit)
  (define races '((7 9) (15 40) (30 200)))
  (check-equal? (total-ways races) 288))


(module+ main
  (define races (parse-data))
  (total-ways races))
