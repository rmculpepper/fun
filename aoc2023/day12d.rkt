#lang racket

(define (memoize f)
  (let ([table (make-hash)])
    (lambda args
      (hash-ref! table args (lambda () (apply f args))))))

(define-syntax-rule (define/memo (f x ...) body ...)
  (define f (memoize (lambda (x ...) body ...))))

;; type Clu = (NEListof Char)
(define (clu-len cs) (length cs))
(define (clu-cdr cs) (cdr cs))

(define (count-char s findc)
  (for/sum ([c (in-string s)]) (if (eqv? c findc) 1 0)))

;; type Row = (list (Listof Clu) (Listof Nat))

;; parse-line : Nat String -> Row
(define (parse-line xp line)
  (match line
    [(regexp #rx"^([?#.]+)[ ]+([0-9,]+)$" (list _ data nums))
     (define datax (string-join (make-list xp data) "?"))
     (define clu-ss (string-split datax #rx"[.]+"))
     (define clus (map string->list clu-ss))
     (define numsx (string-join (make-list xp nums) ","))
     (define ns (map string->number (string-split numsx ",")))
     (list clus ns)]))

;; ----

(define (solve row)
  (count-ways (car row) (cadr row)))

(define/memo (count-ways clus ns)
  (match clus
    [(cons clu1 clus)
     (clu1-ways clu1 clus ns)]
    ['() (if (null? ns) 1 0)]))

(define (clu1-ways clu1 clus ns)
  (match clu1
    [(cons #\# clu1-rest)
     (clu1dam-ways clu1 clus ns)]
    [(cons #\? clu1-rest)
     (+ (clu1dam-ways (cons #\# clu1-rest) clus ns)
        (clu1-ways clu1-rest clus ns))]
    [(list)
     (count-ways clus ns)]))

(define (clu1dam-ways clu1 clus-rest ns)
  ;; PRE: clu1 starts with damage (#)
  (define clu1-len (length clu1))
  (match ns
    [(list)
     0]
    [(cons n1 ns-rest)
     (cond [(< clu1-len n1)
            0]
           [(= clu1-len n1)
            (count-ways clus-rest ns-rest)]
           [else
            (match (drop clu1 n1)
              [(cons #\# _) 0]
              [(cons #\? clu1-rest) (count-ways (cons clu1-rest clus-rest) ns-rest)]
              [(list) (count-ways clus-rest ns-rest)])])]))

(module+ test
  (require rackunit)

  (check-equal? (solve (parse-line 1 "???.### 1,1,3")) 1)
  (check-equal? (solve (parse-line 1 ".??..??...?##. 1,1,3")) 4)
  (check-equal? (solve (parse-line 1 "?#?#?#?#?#?#?#? 1,3,1,6")) 1)
  (check-equal? (solve (parse-line 1 "????.#...#... 4,1,1")) 1)
  (check-equal? (solve (parse-line 1 "????.######..#####. 1,6,5")) 4)
  (check-equal? (solve (parse-line 1 "?###???????? 3,2,1")) 10)

  (when #f
    (check-equal? (solve (parse-line 5 "???.### 1,1,3")) 1)
    (check-equal? (solve (parse-line 5 ".??..??...?##. 1,1,3")) 16384)
    (check-equal? (solve (parse-line 5 "?#?#?#?#?#?#?#? 1,3,1,6")) 1)
    (check-equal? (solve (parse-line 5 "????.#...#... 4,1,1")) 16)
    (check-equal? (solve (parse-line 5 "????.######..#####. 1,6,5")) 2500)
    (check-equal? (solve (parse-line 5 "?###???????? 3,2,1")) 506250))

  (void))

(module+ main
  (define lines (port->lines))
  ;; part1
  (for/sum ([line (in-list lines)])
    (solve (parse-line 1 line)))
  ;; part2
  (for/sum ([line (in-list lines)])
    (solve (parse-line 5 line))))
