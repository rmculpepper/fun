#lang racket

;; symbol-locs : Hash[Nat => Hash[Nat => (U #t (Box (Listof Nat)))]]
;; Map line index to hash that maps char index to box (for gear) or #t (else).
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

;; Pass1: Note all symbol locations, create boxes for each gear.

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

;; Pass2: For each number, check for adjacent symbol.
;; If adjacent to gear, add to gearbox.  Note: avoid short-circuiting `or`
;; in adjacency check, else might miss adding number to gearbox.

;; pass2 : (Listof String) -> Nat
(define (pass2 lines)
  (for/sum ([line (in-list lines)] [li (in-naturals)])
    (pass2-line line li)))

;; pass2-line : String Nat -> Nat
(define (pass2-line line li)
  (let loop ([start 0])
    (match (regexp-match-positions #rx"[0-9]+" line start)
      [(list (cons nstart nend))
       (define n (string->number (substring line nstart nend)))
       (+ (if (adjacent-to-symbol li nstart nend n) n 0)
          (loop nend))]
      [#f 0])))

;; adjacent-to-symbol : Nat Nat Nat Nat -> Boolean
;; Returns #t if adjacent to symbol. Adds n to gearbox of adjacent gear.
(define (adjacent-to-symbol li cstart cend n)
  ;; reduce : (Listof (U Boolean (Box (Listof Nat)))) -> Boolean
  (define (reduce v)
    (define vs (flatten v))
    (for ([b (in-list vs)] #:when (box? b))
      (set-box! b (cons n (unbox b))))
    (and (ormap values vs) #t))
  (reduce (list
           ;; check same line
           (list (check-symbol li (sub1 cstart))
                 (check-symbol li cend))
           ;; check lines above and below
           (for/list ([check-li (in-list (list (sub1 li) (add1 li)))])
             (for/list ([check-ci (in-range (sub1 cstart) (add1 cend))])
               (check-symbol check-li check-ci))))))

;; pass3 : -> Nat
(define (pass3)
  (for/sum ([(li lih) (in-hash symbol-locs)])
    (for/sum ([(ci b) (in-hash lih)] #:when (box? b))
      (if (= 2 (length (unbox b))) (apply * (unbox b)) 0))))

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
  (check-equal? (pass2 example) 4361)
  (check-equal? (pass3) 467835))

(module+ main
  (define lines (port->lines))
  (pass1 lines)
  (pass2 lines)
  (pass3))
