#lang racket

(define (north-load rows)
  (define cols (transpose-pattern rows))
  (apply + (map north-load/col cols)))

(define (north-load/col col)
  (define len (string-length col))
  (let loop ([skip 0])
    (match (regexp-match #rx"^([.O]*)" col skip)
      [(list _ moves)
       (define os (filter (lambda (c) (eqv? c #\O)) (string->list moves)))
       (+ (for/sum ([o (in-list os)] [level (in-range (- len skip) 0 -1)]) level)
          (match (regexp-match-positions #rx"[#]" col skip)
            [(list (cons found after)) (loop after)]
            [_ 0]))])))

(define (stet-load rows)
  (for/sum ([row (in-list rows)] [level (in-range (length rows) 0 -1)])
    (* level (count (lambda (c) (eqv? c #\O)) (string->list row)))))

(define (transpose-pattern lines)
  (define w (string-length (car lines)))
  (build-list w (lambda (i) (list->string (map (lambda (s) (string-ref s i)) lines)))))

(struct grid (vec w h tx? flipi?))
(define (grid-ref g i j)
  (match-define (grid vec w h tx? flipi?) g)
  (let ([i (if flipi? (- w i 1) i)])
    (let-values ([(i j w) (if tx? (values j i h) (values i j w))])
      (vector-ref vec (+ i (* w j))))))
(define (grid-set! g i j v)
  (match-define (grid vec w h tx? flipi?) g)
  (let ([i (if flipi? (- w i 1) i)])
    (let-values ([(i j w) (if tx? (values j i h) (values i j w))])
      (vector-set! vec (+ i (* w j)) v))))
(define (strings->grid rows)
  (define w (string-length (car rows)))
  (define h (length rows))
  (define vec (make-vector (* w h)))
  (for ([j (in-naturals)] [row (in-list rows)])
    (for ([i (in-naturals)] [c (in-string row)])
      (vector-set! vec (+ i (* w j)) c)))
  (grid vec w h #f #f))
(define (grid-share-transpose g)
  (match-define (grid vec w h tx? #f) g)
  (grid vec h w (not tx?) #f))
(define (grid-share-flipi g)
  (match-define (grid vec w h tx? #f) g)
  (grid vec w h tx? #t))
(define (grid->strings g)
  (for/list ([j (in-range (grid-h g))])
    (list->string (for/list ([i (in-range (grid-w g))])
                    (grid-ref g i j)))))
(define (grid-snapshot g)
  (vector-copy (grid-vec g)))
(define (grid-snapshot/v0 g)
  ;; PRE: g has just done a move-east
  (define vec (grid-vec g))
  (define len (vector-length vec))
  ;; list of ( { n.s nOs n#s } ... )
  (list->bytes
   (let loop ([i 0])
     (cond [(< i len)
            (define-values (i1 dots) (count-run vec len i #\.))
            (define-values (i2 os) (count-run vec len i1 #\O))
            (define-values (i3 sharps) (count-run vec len i2 #\#))
            (list* dots os sharps (loop i3))]
           [else null]))))

(define (count-run vec len i runc)
  (let loop ([i i] [run 0])
    (cond [(< i len)
           (if (eqv? (vector-ref vec i) runc)
               (loop (add1 i) (add1 run))
               (values i run))]
          [else (values i run)])))

(define (run-cycles rows n)
  ;; North, West, South, East
  (define g (strings->grid rows))
  (define gt (grid-share-transpose g))
  (define gf (grid-share-flipi g))
  (define gtf (grid-share-flipi gt))
  (define seen (make-hash))
  (define (one-cycle)
    (move-left gt)
    (move-left g)
    (move-left gtf)
    (move-left gf))
  (define-values (iters-done cyclen)
    (let/ec escape
      (for ([i (in-range n)] [iter (in-naturals 1)])
        (one-cycle)
        (define snapshot (grid-snapshot g))
        (when #t
          (cond [(hash-ref seen snapshot #f)
                 => (lambda (prev-iter)
                      (escape iter (- iter prev-iter)))]
                [else
                 (hash-set! seen snapshot iter)])))
      (values n n)))
  (define need-iters (remainder (- n iters-done) cyclen))
  (for ([i (in-range need-iters)])
    (one-cycle))
  (grid->strings g))

(define (move-left g)
  (define w (grid-w g))
  (for ([j (in-range (grid-h g))])
    (let loop ([starti 0])
      (define-values (endi os) (find-end/os g j starti w))
      (for ([i (in-range starti (+ starti os))])
        (grid-set! g i j #\O))
      (for ([i (in-range (+ starti os) endi)])
        (grid-set! g i j #\.))
      (when (< endi w) (loop (add1 endi))))))

(define (find-end/os g j starti w)
  (let loop ([starti starti] [os 0])
    (cond [(< starti w)
           (case (grid-ref g starti j)
             [(#\#) (values starti os)]
             [(#\O) (loop (add1 starti) (add1 os))]
             [else (loop (add1 starti) os)])]
          [else (values w os)])))


(module+ test
  (require rackunit)
  (define rows
    '("O....#...."
      "O.OO#....#"
      ".....##..."
      "OO.#O....O"
      ".O.....O#."
      "O.#..O.#.#"
      "..O..#O..O"
      ".......O.."
      "#....###.."
      "#OO..#...."))
  (check-equal? (north-load rows) 136)

  (check-equal?
   (run-cycles rows 1)
   '(".....#...."
     "....#...O#"
     "...OO##..."
     ".OO#......"
     ".....OOO#."
     ".O#...O#.#"
     "....O#...."
     "......OOOO"
     "#...O###.."
     "#..OO#...."))

  (check-equal?
   (run-cycles rows 2)
   '(".....#...."
     "....#...O#"
     ".....##..."
     "..O#......"
     ".....OOO#."
     ".O#...O#.#"
     "....O#...O"
     ".......OOO"
     "#..OO###.."
     "#.OOO#...O"))

  (check-equal?
   (run-cycles rows 3)
   '(".....#...."
     "....#...O#"
     ".....##..."
     "..O#......"
     ".....OOO#."
     ".O#...O#.#"
     "....O#...O"
     ".......OOO"
     "#...O###.O"
     "#.OOO#...O"))

  (define rows* (run-cycles rows 1000000000))
  (check-equal? (stet-load rows*) 64))

(module+ main
  (define rows (port->lines))

  ;; part 1
  (north-load rows)

  ;; part 2
  (define rows* (run-cycles rows 1000000000))
  (stet-load rows*)
  (void))
