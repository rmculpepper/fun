#lang racket

;; type Rngs = (Listof Rng)

(struct rng (start run) #:prefab)

(define blank-line-rx #rx"^[ ]*$")

;; read-data : -> (values Rngs (Listof Map))
(define (read-data)
  (define seeds (seed-ranges (read-seeds)))
  (match (regexp-match blank-line-rx (read-line)) [(list _) (void)])
  (define maps (read-maps))
  (values seeds maps))

;; read-seeds : -> (Listof Rng)
(define (read-seeds)
  (match (regexp-match "^seeds:[ ]*([0-9 ]+)" (read-line))
    [(list _ nums)
     (map string->number (string-split (string-trim nums)))]))

;; seed-ranges : (Listof Nat) -> Rngs
(define (seed-ranges srs)
  (match srs
    [(list* start run rest)
     (cons (rng start run) (seed-ranges rest))]
    [(list) null]))

;; type Map = (list Symbol Symbol (Rng -> Rngs))

;; read-maps : -> (Listof Map)
(define (read-maps)
  (define m (read-map))
  (if m (cons m (read-maps)) null))

;; read-maps : -> #f or Map
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

;; interpret-mappings : (Listof (list Nat Nat Nat)) -> Rng -> Rngs
(define ((interpret-mappings es) r)
  (match-define (rng start r-run) r)
  (define end (+ start r-run))
  (let loop ([start start] [end end] [es es])
    (if (< start end)
        (match es
          [(cons (list dest-start src-start e-run) es)
           (define (remap start end)
             (cond [(< start end)
                    (list (rng (+ dest-start (- start src-start))
                               (- end start)))]
                   [else null]))
           (define src-end (+ src-start e-run))
           (append
            ;; part before e
            (loop start (min end src-start) es)
            ;; part within e
            (remap (max start src-start) (min end src-end))
            ;; part after e
            (loop (max start src-end) end es))]
          ['() (list (rng start (- end start)))])
        null)))

;; ----

(struct state (type value) #:prefab)

(define (location-state? s) (eq? 'location (state-type s)))

;; get-next : Maps State -> (Listof State)
(define (get-next maps st)
  (match-define (state type value) st)
  (append*
   (for/list ([m (in-list maps)])
     (match m
       [(list from to mapper)
        (cond [(equal? type from)
               (for/list ([new-value (in-list (mapper value))])
                 (state to new-value))]
              [else null])]))))

;; find-locations : Maps (Listof Rng) -> (Listof Rng)
(define (find-locations maps srs)
  (define init-states (for/list ([sr (in-list srs)]) (state 'seed sr)))
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

;; rngs-min : (Listof Rng) -> Nat
(define (rngs-min rngs)
  (apply min (map rng-start rngs)))

(module+ test
  (require rackunit)
  (define-values (seeds maps)
    (with-input-from-file "input/ex05.txt"
      (lambda () (read-data))))
  (check-equal? (rngs-min (find-locations maps seeds)) 46))

(module+ main
  (define-values (srs maps) (read-data))
  (define locs (find-locations maps srs))
  (rngs-min locs))

