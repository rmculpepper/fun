#lang racket

;; A Pattern is (Listof String)

(define (lines->patterns lines)
  (if (null? lines)
      null
      (let-values ([(pattern rest) (splitf-at lines (lambda (s) (> (string-length s) 0)))])
        (let ([rest (if (pair? rest) (cdr rest) rest)])
          (cons pattern (lines->patterns rest))))))

(define (transpose-pattern lines)
  (define w (string-length (car lines)))
  (build-list w (lambda (i) (list->string (map (lambda (s) (string-ref s i)) lines)))))

(define (find-hreflect lines factor)
  (for/or ([li (in-range 1 (length lines))])
    (and (for/and ([i (in-range (sub1 li) -1 -1)]
                   [j (in-range li (length lines))])
           (equal? (list-ref lines i)
                   (list-ref lines j)))
         (* factor li))))

(define (find-reflect lines)
  (or (find-hreflect lines 100)
      (find-hreflect (transpose-pattern lines) 1)))

(module+ test
  (define p1
    '("#.##..##."
      "..#.##.#."
      "##......#"
      "##......#"
      "..#.##.#."
      "..##..##."
      "#.#.##.#."))
  (define p2
    '("#...##..#"
      "#....#..#"
      "..##..###"
      "#####.##."
      "#####.##."
      "..##..###"
      "#....#..#"))

  (find-reflect p1)
  (find-reflect p2))

(module+ main
  (define lines (port->lines))
  (define patterns (lines->patterns lines))
  ;; part 1
  (for/sum ([pattern patterns])
    (find-reflect pattern)))
