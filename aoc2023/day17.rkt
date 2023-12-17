#lang racket

(struct grid (w h vec))
(define (make-strings-grid lines)
  (define h (length lines))
  (define w (string-length (car lines)))
  (define vec
    (list->vector (for/list ([line (in-list lines)])
                    (list->vector (for/list ([c (in-string line)])
                                    (string->number (string c)))))))
  (grid w h vec))
(define (make-grid w h fill)
  (build-vector h (lambda (j) (make-vector w fill))))
(define (grid-ref g i j)
  (if (and (<= 0 i) (< i (grid-w g)) (<= 0 j) (< j (grid-h g)))
      (vector-ref (vector-ref (grid-vec g) j) i)
      +nan.0))
(define (grid-set! g i j v)
  (vector-set! (vector-ref (grid-vec g) j) i v))

;; type Node = (state Nat Nat {-1,0,1} {-1,0,1} Nat)
;;   i, j are positions; di, dj direction; steps taken in that direction
(struct node (i j di dj n) #:prefab)

;; type State = (cons Node Nat), node with sum for a particular path

;; Problem: Compute min sum going from (0, 0) to (i, j) in direction (di, dj)
;; having gone in that driection for the last n steps.

(define (process g mindir maxdir)
  (define w (grid-w g))
  (define h (grid-h g))

  (define (nxnode i j di dj n)
    (node (+ i di) (+ j dj) di dj n))
  (define (nxstate i j di dj n sum)
    (define nd (nxnode i j di dj n))
    (cons nd (+ sum (grid-ref g (node-i nd) (node-j nd)))))

  (define init-states
    (list (nxstate 0 0 1 0 1 0)
          (nxstate 0 0 0 1 1 0)))

  (define (ok-node? s)
    (let ([i (node-i s)] [j (node-j s)])
      (and (<= 0 i) (< i w) (<= 0 j) (< j h))))
  (define (ok-state? s) (ok-node? (car s)))

  (define (end-node? nd)
    (and (= (node-i nd) (sub1 w))
         (= (node-j nd) (sub1 h))
         (>= (node-n nd) mindir)))

  (define (state-next s)
    (match-define (cons (node i j di dj n) sum) s)
    (define can-turn? (>= n mindir))
    (define can-go-straight? (< n maxdir))
    (filter values
            (list
             ;; go straight
             (and can-go-straight?
                  (nxstate i j di dj (add1 n) sum))
             ;; turn left or right
             (and can-turn?
                  (let ([di dj] [dj di])
                    (nxstate i j di dj 1 sum)))
             (and can-turn?
                  (let ([di (- dj)] [dj (- di)])
                    (nxstate i j di dj 1 sum))))))

  (define best (make-hash)) ;; Node => Nat

  (let loop ([states init-states])
    (when (pair? states)
      (define next1 (append* (map state-next states)))
      (define next2
        (for/fold ([acc null]) ([s (in-list next1)])
          (match-define (cons nd sum) s)
          (cond [(< sum (hash-ref best nd +inf.0))
                 (hash-set! best nd sum)
                 (cons s acc)]
                [else acc])))
      (loop next2)))

  (define end (for/list ([(nd sum) (in-hash best)]
                         #:when (end-node? nd))
                sum))
  (apply min end))

(module+ test
  (require rackunit)
  (define lines
    '("2413432311323"
      "3215453535623"
      "3255245654254"
      "3446585845452"
      "4546657867536"
      "1438598798454"
      "4457876987766"
      "3637877979653"
      "4654967986887"
      "4564679986453"
      "1224686865563"
      "2546548887735"
      "4322674655533"))
  (define g (make-strings-grid lines))
  (process g 0 3)
  (process g 4 10)
  (void))

(module+ main
  (define lines (port->lines))
  (define g (make-strings-grid lines))
  ;; part 1
  (process g 0 3)
  ;; part 2
  (process g 4 10)
  (void))
