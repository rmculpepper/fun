#lang racket

(struct dig (dir steps color) #:prefab)

;; parse-line : String -> Dig
(define (parse-line line)
  (match (regexp-match #rx"^([UDLR]) ([0-9]+) [(]#([0-9a-f]+)[)]$" line)
    [(list _ dir steps-s color-s)
     (dig dir (string->number steps-s) (string->number color-s 16))]))

(define (find-outline digs x0 y0)
  (define g (make-hash)) ;; (cons Int Int) => (Listof Dir)
  (define c (make-hash)) ;; (cons Int Int) => Color
  (for/fold ([x x0] [y y0]) ([d digs])
    (match-define (dig dir steps color) d)
    (define-values (dx dy) (interpret-dir dir))
    (for ([i (in-range 0 (add1 steps))])
      (hash-cons! g (cons (+ x (* i dx)) (+ y (* i dy))) dir))
    (values (+ x (* steps dx)) (+ y (* steps dy))))
  (hash-update! g (cons 0 0) (lambda (dirs) (remove-duplicates (reverse dirs))))
  g)

(define (hash-cons! h k v)
  (hash-update! h k (lambda (vs) (cons v vs)) null))

(define (find-interior g)
  (define interior (make-hash)) ;; (cons Int Int) => #t
  (define-values (minx miny maxx maxy) (graph-limits g))
  (for ([y (in-range miny (add1 maxy))])
    (define inside? #f)
    (define lastv #f)
    (for ([x (in-range minx (add1 maxx))])
      (define (flip) (set! inside? (not inside?)))
      (cond [(get-lr-vert (hash-ref g (cons x y) null))
             => (lambda (vert)
                  (case vert
                    [(#\|) (flip)]
                    [(#\-) (void)]
                    [(#\L) (set! lastv 'up)]
                    [(#\J) (when (eq? lastv 'down) (flip)) (set! lastv #f)]
                    [(#\7) (when (eq? lastv 'up) (flip)) (set! lastv #f)]
                    [(#\F) (set! lastv 'down)]))]
            [else (when inside? (hash-set! interior (cons x y) #t))])))
  interior)

(define (get-lr-vert dirs)
  (match (reverse dirs)
    [(or '("D") '("U")) #\|]
    [(or '("D" "R") '("L" "U")) #\L]
    [(or '("D" "L") '("R" "U")) #\J]
    [(or '("U" "R") '("L" "D")) #\F]
    [(or '("U" "L") '("R" "D")) #\7]
    [(or '("L") '("R")) #\-]
    ['() #f]))

(define (graph-limits g)
  (define-values (minx maxx)
    (let ([xs (map car (hash-keys g))]) (values (apply min xs) (apply max xs))))
  (define-values (miny maxy)
    (let ([ys (map cdr (hash-keys g))]) (values (apply min ys) (apply max ys))))
  (values minx miny maxx maxy))

(define (interpret-dir dir)
  (case dir
    [("U") (values 0 -1)]
    [("D") (values 0 1)]
    [("L") (values -1 0)]
    [("R") (values 1 0)]))

(module+ main
  (define digs (map parse-line (port->lines)))
  (define outline (find-outline digs 0 0))
  (define interior (find-interior outline))
  ;; part 1
  (hash-count outline)
  (+ (hash-count outline) (hash-count interior))

  (define-values (minx miny maxx maxy) (graph-limits outline))
  (printf "~s to ~s\n" (list minx miny) (list maxx maxy))
  (when #f
    (for ([y (in-range miny (add1 maxy))])
      (for ([x (in-range minx (add1 maxx))])
        (cond [(and (zero? x) (zero? y))
               (write-string "■")]
              [(and (hash-ref outline (cons x y) #f)
                    (hash-ref interior (cons x y) #f))
               (write-char #\X)]
              [(hash-ref outline (cons x y) #f)
               (write-char #\#)]
              [(hash-ref interior (cons x y) #f)
               (write-char #\.)]
              [else (write-char #\space)]))
      (newline)))
  (void))
