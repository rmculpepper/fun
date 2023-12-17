#lang racket

(struct grid (w h vec))
(define (strings->grid strs)
  (grid (string-length (car strs)) (length strs) (list->vector strs)))
(define (grid-ref g i j)
  (string-ref (vector-ref (grid-vec g) j) i))

;; INV: (+ (abs di) (abs dj)) is always 1

(define (horizontal? di dj)
  (not (zero? di)))
(define (vertical? di dj)
  (not (zero? dj)))

(define (traverse g i j di dj)
  (define w (grid-w g))
  (define h (grid-h g))
  (define seen (make-hash))
  (let loop ([i i] [j j] [di di] [dj dj])
    (define (continue [di di] [dj dj])
      (loop (+ i di) (+ j dj) di dj))
    (when (and (<= 0 i) (< i w) (<= 0 j) (< j h))
      (define key (list* di dj i j))
      (unless (hash-ref seen key #f)
        (hash-set! seen key #t)
        (case (grid-ref g i j)
          [(#\.) (continue)]
          [(#\|) (cond [(horizontal? di dj)
                        (continue 0 -1)
                        (continue 0 1)]
                       [else (continue)])]
          [(#\-) (cond [(vertical? di dj)
                        (continue -1 0)
                        (continue 1 0)]
                       [else (continue)])]
          [(#\/) (continue (- dj) (- di))]
          [(#\\) (continue dj di)]
          ))))
  (hash-count
   (for/fold ([h (hash)]) ([k (in-hash-keys seen)])
     (hash-set h (cddr k) #t))))

(module+ test
  (require rackunit)
  (define lines
    '(".|...\\...."
      "|.-.\\....."
      ".....|-..."
      "........|."
      ".........."
      ".........\\"
      "..../.\\\\.."
      ".-.-/..|.."
      ".|....-|.\\"
      "..//.|...."))
  (define g (strings->grid lines))
  (check-equal? (hash-count (traverse g 0 0 1 0))
                46))

(module+ main
  (define lines (port->lines))
  (define g (strings->grid lines))
  ;; part 1
  (traverse g 0 0 1 0)
  ;; part 2
  (define w (grid-w g))
  (define h (grid-h g))
  (apply max
         (flatten
          (list
           (for/list ([i (in-range (grid-w g))])
             (list (traverse g i 0 0 1)
                   (traverse g i (sub1 h) 0 -1)))
           (for/list ([j (in-range (grid-h g))])
             (list (traverse g 0 j 1 0)
                   (traverse g (sub1 w) j -1 0))))))
  (void))
