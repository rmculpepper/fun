#lang racket

(define blank-line-rx #rx"^[ ]*$")

;; read-data : -> (values (Listof Nat) (Listof (list Symbol Symbol (Nat -> Nat))))
(define (read-data)
  (define seeds (read-seeds))
  (match (regexp-match blank-line-rx (read-line)) [(list _) (void)])
  (define maps (read-maps))
  (values seeds maps))

;; read-seeds : -> (Listof Nat)
(define (read-seeds)
  (match (regexp-match "^seeds:[ ]*([0-9 ]+)" (read-line))
    [(list _ nums)
     (map string->number (string-split (string-trim nums)))]))

;; read-maps : -> (Listof (list Symbol Symbol (Nat -> Nat)))
(define (read-maps)
  (define m (read-map))
  (if m (cons m (read-maps)) null))

;; read-maps : -> (list Symbol Symbol (Nat -> Nat))
(define (read-map)
  (match (read-line)
    [(? eof-object?)
     #f]
    [(regexp "^([a-z]+)-to-([a-z]+) map:$" (list _ from-s to-s))
     (define from (string->symbol from-s))
     (define to (string->symbol to-s))
     (define mappings
       (let loop ()
         (match (read-line)
           [(regexp #rx"^([0-9]+)[ ]*([0-9]+)[ ]*([0-9]+)[ ]*$"
                    (list _ start end dest))
            (cons
             (list (string->number start) (string->number end) (string->number dest))
             (loop))]
           [(regexp blank-line-rx)
            null]
           [(? eof-object?)
            null])))
     (list from to (interpret-mappings mappings))]))

;; interpret-mappings : (Listof (list Nat Nat Nat)) -> Nat -> Nat
(define (interpret-mappings m)
  (define (mapper n)
    (or (for/or ([e (in-list m)])
          (match-define (list dest-start src-start run) e)
          (and (<= src-start n) (< n (+ src-start run))
               (+ dest-start (- n src-start))))
        n))
  mapper)

;; ----

(struct state (type value) #:prefab)

(define (location-state? s) (eq? 'location (state-type s)))

;; get-next : Maps State -> (Listof State)
(define (get-next maps st)
  (match-define (state type value) st)
  (filter values
          (for/list ([m (in-list maps)])
            (match m
              [(list from to mapper)
               (cond [(equal? type from)
                      (state to (mapper value))]
                     [else #f])]))))

;; find-locations : Maps (Listof Nat) -> (Listof Nat)
(define (find-locations maps seeds)
  (define init-states (for/list ([seed (in-list seeds)]) (state 'seed seed)))
  ;; (printf "~v\n" init-states)
  (let loop ([states init-states] [seen-states null]) ;; PRE: states exclude seen-states
    (cond [(null? states)
           null]
          [else
           (define all-next (append* (map (lambda (s) (get-next maps s)) states)))
           ;; (printf "~v\n" all-next)
           (define-values (loc-states other-states)
             (partition location-state? all-next))
           (append (map state-value loc-states)
                   (loop (remove* seen-states other-states)
                         (append states seen-states)))])))


;; seed-ranges->seeds : (Listof Nat) -> (Listof Nat)
(define (seed-ranges->seeds srs)
  (match srs
    [(list* start run rest)
     (append (range start (+ start run))
             (seed-ranges->seeds rest))]
    [(list) null]))

(module+ test
  (require rackunit)
  (define-values (seeds maps)
    (with-input-from-file "input/ex05.txt"
      (lambda () (read-data))))
  (check-equal? (apply min (find-locations maps seeds)) 35))

(module+ main
  (define-values (seeds1 maps)
    (read-data))
  (define locs1 (find-locations maps seeds1))
  ;; part 1
  (apply min locs1)

  ;; part 2
  (define seeds2 (seed-ranges->seeds seeds1))
  (define locs2 (find-locations maps seeds2))
  (apply min locs2))

