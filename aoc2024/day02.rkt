#lang racket
(require racket/port)

(define (read-data [in (current-input-port)])
  (for/list ([line (in-lines in)])
    (line->report line)))

(define (line->report line)
  (sequence->list (in-port read (open-input-string line))))

(define (safe? ns0)
  (define n0 (car ns0))
  (define ns (cdr ns0))
  (or (safe* n0 ns +1 +3)
      (safe* n0 ns -3 -1)))

(define (safe* n0 ns mindiff maxdiff)
  (match ns
    ['() #t]
    [(cons n1 ns)
     (and (<= (+ n0 mindiff) n1 (+ n0 maxdiff))
          (safe* n1 ns mindiff maxdiff))]))

(printf "Part 1\n")
(define reports (read-data))
(for/sum ([report reports])
  (if (safe? report) 1 0))

(define (safe2? ns)
  (define n (length ns))
  (for/or ([i (in-range n)])
    (safe? (remove-at ns i))))

(define (remove-at ns ri)
  (for/list ([n (in-list ns)] [i (in-naturals)] #:when (not (= i ri))) n))

(printf "Part 2\n")
(for/sum ([report reports])
  (if (safe2? report) 1 0))
