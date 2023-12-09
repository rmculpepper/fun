#lang racket

(define (parse-lines lines)
  (map parse-line lines))

(define (parse-line line)
  (map string->number (string-split (string-trim line))))

;; extrapolate : (Listof Nat) -> Nat
(define (extrapolate nums)
  (if (andmap zero? nums)
      0
      (+ (last nums) (extrapolate (diffs nums)))))

;; bextrapolate : (Listof Nat) -> Nat
(define (bextrapolate nums)
  (if (andmap zero? nums)
      0
      (- (first nums) (bextrapolate (diffs nums)))))

;; diffs : (NEListof Nat) -> (Listof Nat)
(define (diffs ns)
  (if (pair? (cdr ns))
      (cons (- (second ns) (first ns)) (diffs (cdr ns)))
      null))

(module+ test
  (require rackunit)
  (check-equal? (extrapolate '(0 3 6 9 12 15)) 18)
  (check-equal? (extrapolate '(1 3 6 10 15 21)) 28)
  (check-equal? (extrapolate '(10 13 16 21 30 45)) 68))

(module+ main
  (define lines (port->lines))
  (define seqs (map parse-line lines))
  ;; part 1
  (for/sum ([seq (in-list seqs)])
    (extrapolate seq))
  ;; part 2
  (for/sum ([seq (in-list seqs)])
    (bextrapolate seq))
  )
