#lang racket

;; A Point is (point Nat Nat)
(struct point (x y) #:prefab)

;; parse-graph : (Listof String) -> (values Nat Nat (Listof Point) (Listof Nat) (Listof Nat))
(define (parse-graph lines)
  (define h (length lines))
  (define w (string-length (car lines)))
  (define galaxies
    (append*
     (for/list ([line (in-list lines)] [y (in-naturals)])
       (for/list ([c (in-string line)] [x (in-naturals)] #:when (eqv? c #\#))
         (point x y)))))
  (define wide-xs
    (let ([gxs (map point-x galaxies)])
      (filter (lambda (x) (not (member x gxs))) (range w))))
  (define wide-ys
    (let ([gys (map point-y galaxies)])
      (filter (lambda (y) (not (member y gys))) (range h))))
  (values w h galaxies wide-xs wide-ys))

;; dist : Point Point (Listof Nat) (Listof Nat) -> Nat
;; Manhattan distance where some coordinate values are "wide".
(define (dist p1 p2 wide-xs wide-ys)
  (+ (dist* (point-x p1) (point-x p2) wide-xs)
     (dist* (point-y p1) (point-y p2) wide-ys)))

;; dist* : Nat Nat (Listof Nat) -> Nat
(define (dist* x1 x2 wide-xs)
  (for/sum ([x (in-range (min x1 x2) (max x1 x2))])
    (if (member x wide-xs) #;2 #e1e6 1)))

(module+ main
  (define lines (port->lines))
  (define-values (w h galaxies wide-xs wide-ys) (parse-graph lines))
  ;; part 1
  (let loop ([gs galaxies])
    (cond [(null? gs)
           0]
          [else
           (define g0 (car gs))
           (+ (for/sum ([g (in-list (cdr gs))])
                (dist g0 g wide-xs wide-ys))
              (loop (cdr gs)))]))
  (void))
