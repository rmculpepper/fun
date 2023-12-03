#lang racket

;; symbol-locs : Hash[Nat => Hash[Nat => (U #t (Box (Listof Nat)))]]
(define symbol-locs (make-hash))

;; set-symbol! : Nat Nat Boolean -> Void
(define (set-symbol! li ci gear?)
  (hash-update! symbol-locs li
                (lambda (cih)
                  (hash-set cih ci (if gear? (box null) #t)))
                '#hash()))

;; check-symbol : Nat Nat -> (U Boolean (Box (Listof Nat)))
(define (check-symbol li ci)
  (define lih (hash-ref symbol-locs li #f))
  (and lih (hash-ref lih ci #f)))


;; pass1 : (Listof String) -> Void
(define (pass1 lines)
  (for ([line (in-list lines)] [li (in-naturals)])
    (pass1-line line li)))

;; pass1-line : String Nat -> Void
(define (pass1-line line li)
  (for ([c (in-string line)] [ci (in-naturals)])
    (cond [(symbol-char? c)
           (set-symbol! li ci (eqv? c #\*))]
          [(char-numeric? c)
           (void)]
          [(member c '(#\.))
           (void)]
          [else (eprintf "weird char: ~e\n" c)])))

;; symbol-char? : Char -> Boolean
(define (symbol-char? c)
  (for/or ([sc (in-string "~!@#$%^&*_+-=/")]) ;; ???
    (eqv? c sc)))


;; pass2 : (Listof String) -> Nat
(define (pass2 lines)
  (for/sum ([line (in-list lines)] [li (in-naturals)])
    (pass2-line line li)))

;; pass2-line : String Nat -> Nat
(define (pass2-line line li)
  (let loop ([start 0])
    (match (regexp-match-positions #rx"[0-9]+" line start)
      [(list (cons nstart nend))
       (+ (if (adjacent-to-symbol? li nstart nend)
              (string->number (substring line nstart nend))
              0)
          (loop nend))]
      [#f 0])))

;; adjacent-to-symbol? : Nat Nat Nat -> Boolean
(define (adjacent-to-symbol? li cstart cend)
  (or
   ;; check same line
   (or (check-symbol li (sub1 cstart))
       (check-symbol li cend))
   ;; check lines above and below
   (for/or ([check-li (in-list (list (sub1 li) (add1 li)))])
     (for/or ([check-ci (in-range (sub1 cstart) (add1 cend))])
       (check-symbol check-li check-ci)))))

(module+ test
  (require rackunit)

  (define example
    '("467..114.."
      "...*......"
      "..35..633."
      "......#..."
      "617*......"
      ".....+.58."
      "..592....."
      "......755."
      "...$.*...."
      ".664.598.."))

  (pass1 example)
  (check-equal? (pass2 example) 4361))

(module+ main
  (define lines (port->lines))
  (pass1 lines)
  (pass2 lines))
