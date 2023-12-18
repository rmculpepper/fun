#lang racket

(struct dig (dir steps) #:prefab)

;; parse-line : String -> Dig
(define (parse-line line)
  (match (regexp-match #rx" [(]#([0-9a-f]+)([0-9a-f])[)]$" line)
    [(list _ steps-s dir-s)
     (dig (case dir-s
            [("0") "R"]
            [("1") "D"]
            [("2") "L"]
            [("3") "U"])
          (string->number steps-s 16))]))

(struct lineseg (x1 y1 x2 y2 dir stepi) #:prefab)
;; INV: x1 = x2 or y1 = y2

(define (find-outline digs x0 y0)
  (define g (make-hash)) ;; Int => (Listof LineSeg)
  (for/fold ([x x0] [y y0]) ([d digs] [stepi (in-naturals)])
    (match-define (dig dir steps) d)
    (define-values (dx dy) (interpret-dir dir))
    (define ls (lineseg x y (+ x (* steps dx)) (+ y (* steps dy)) dir stepi))
    (match ls
      [(lineseg x1 y1 x2 y2 _ _)
       (for ([coord (in-list (remove-duplicates (list x1 y1 x2 y2)))])
         (hash-cons! g coord ls))
       (values x2 y2)]))
  g)

(define (hash-cons! h k v)
  (hash-update! h k (lambda (vs) (cons v vs)) null))

(struct rect (x1 x2 y1 y2) #:prefab)

(define (equiv-classes xs)
  ;; given (x1, x2, ...), produce ({x1}, [x1+1,x2-1], {x2}, ...)
  (define (pointloop xs)
    (match xs
      [(list* x1 x2 xs)
       (cons (list x1 x1)
             (cond [(<= (add1 x1) (sub1 x2))
                    (cons (list (add1 x1) (sub1 x2))
                          (pointloop (cons x2 xs)))]
                   [else (pointloop (cons x2 xs))]))]
      [(list x2)
       (list (list x2 x2))]))
  (pointloop xs))

(define (find-interior g)
  (define interior null) ;; (Listof Rect)
  (define-values (xs ys) (get-sorted-coords g))
  (define xcs (equiv-classes xs))
  (define ycs (equiv-classes ys))
  (for ([yc (in-list ycs)])
    (define y (car yc))
    (define inside? #f)
    (define lastv #f)
    (for ([xc (in-list xcs)])
      (define (flip) (set! inside? (not inside?)))
      (define (this-rect) (rect (car xc) (cadr xc) (car yc) (cadr yc)))
      (define x (car xc))
      (cond [(get-lr-vert (get-dirs g x y))
             => (lambda (vert)
                  (case vert
                    [(#\|) (flip)]
                    [(#\-) (void)]
                    [(#\L) (set! lastv 'up)]
                    [(#\J) (when (eq? lastv 'down) (flip)) (set! lastv #f)]
                    [(#\7) (when (eq? lastv 'up) (flip)) (set! lastv #f)]
                    [(#\F) (set! lastv 'down)])
                  (set! interior (cons (this-rect) interior)))]
            [else (when inside?
                    (set! interior (cons (this-rect) interior)))])))
  interior)

(define (get-dirs g x y)
  (define lss (filter (match-lambda
                        [(lineseg x1 y1 x2 y2 _ _)
                         (and (<= x1 x x2)
                              (<= y1 y y2))])
                      (append (hash-ref g x null)
                              (hash-ref g y null))))
  (define sorted-lss (remove-duplicates (sort lss < #:key lineseg-stepi)))
  (define dirs (map lineseg-dir sorted-lss))
  (if (and (zero? x) (zero? y)) (reverse dirs) dirs))

(define (get-sorted-coords g)
  (define xs (make-hash))
  (define ys (make-hash))
  (for* ([lss (in-hash-values g)] [ls (in-list lss)])
    (match-define (lineseg x1 y1 x2 y2 _ _) ls)
    (hash-set! xs x1 #t)
    (hash-set! xs x2 #t)
    (hash-set! ys y1 #t)
    (hash-set! ys y2 #t))
  (values (sort (hash-keys xs) <)
          (sort (hash-keys ys) <)))

(define (get-lr-vert dirs)
  (match dirs
    [(or '("D") '("U")) #\|]
    [(or '("D" "R") '("L" "U")) #\L]
    [(or '("D" "L") '("R" "U")) #\J]
    [(or '("U" "R") '("L" "D")) #\F]
    [(or '("U" "L") '("R" "D")) #\7]
    [(or '("L") '("R")) #\-]
    ['() #f]))

(define (interpret-dir dir)
  (case dir
    [("U") (values 0 -1)]
    [("D") (values 0 1)]
    [("L") (values -1 0)]
    [("R") (values 1 0)]))

(define (rect-area r)
  (match r [(rect x1 y1 x2 y2) (* (- (add1 x2) x1) (- (add1 y2) y1))]))

(module+ main
  (define digs (map parse-line (port->lines)))
  (define outline (find-outline digs 0 0))
  (define interior (find-interior outline))
  ;; part 2
  (for/sum ([r (in-list interior)]) (rect-area r)))
