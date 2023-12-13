#lang racket
(provide (all-defined-out))

;; type Row = (list Nat (Listof Char) (Listof Nat))

;; parse-line : String -> Row
(define (parse-line line)
  (match line
    [(regexp #rx"^([?#.]+)[ ]+([0-9,]+)$" (list _ data nums))
     (define chars (string->list data))
     (define qs (count (lambda (c) (eqv? c #\?)) chars))
     (list qs chars (map string->number (string-split nums ",")))]))

;; count-damaged : (Listof Char) -> (Listof Nat)
(define (count-damaged data)
  (define raw
    (foldr (lambda (c res)
             (match* [c res]
               [[#\# (cons n ns)] (cons (add1 n) ns)]
               [[#\# (box ns)] (cons 1 ns)]
               [[#\. (? box? b)] b]
               [[#\. (? list? ns)] (box ns)]))
           (box null)
           data))
  (if (box? raw) (unbox raw) raw))

;; instantiate : (Listof Char) -> (Listof (Listof Char))
(define (instantiate data)
  (match data
    ['() (list '())]
    [(cons #\? cs)
     (append (instantiate (cons #\# cs))
             (instantiate (cons #\. cs)))]
    [(cons c cs)
     (for/list ([s (in-list (instantiate cs))])
       (cons c s))]))

;; count-consistent : Row -> Nat
(define (count-consistent row)
  (match row
    [(list _ cs damaged)
     (define candidates (instantiate cs))
     (count (lambda (data) (equal? (count-damaged data) damaged))
            candidates)]))

(module+ test
  (require rackunit)

  (check-equal? (count-consistent (parse-line "???.### 1,1,3")) 1)
  (check-equal? (count-consistent (parse-line ".??..??...?##. 1,1,3")) 4)
  (check-equal? (count-consistent (parse-line "?#?#?#?#?#?#?#? 1,3,1,6")) 1)
  (check-equal? (count-consistent (parse-line "????.#...#... 4,1,1")) 1)
  (check-equal? (count-consistent (parse-line "????.######..#####. 1,6,5")) 4)
  (check-equal? (count-consistent (parse-line "?###???????? 3,2,1")) 10)
  (void))

(module+ main
  (define lines (port->lines))
  (define rows (map parse-line lines))
  (for/sum ([row (in-list rows)])
    (count-consistent row)))
