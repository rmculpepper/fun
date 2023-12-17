#lang racket

(define (hash-string s)
  (for/fold ([init 0]) ([c (in-string s)])
    (define cn (char->integer c))
    (remainder (* 17 (+ init cn)) 256)))

(define (run steps)
  (define boxes (make-vector 256 null))
  (for ([step (in-list steps)])
    (run1 boxes step))
  boxes)

(define (run1 boxes step)
  (match step
    [(regexp #rx"^([a-zA-Z]+)-$" (list _ label))
     (define bi (hash-string label))
     (vector-set! boxes bi (box-remove label (vector-ref boxes bi)))]
    [(regexp #rx"^([a-zA-Z]+)=([0-9]+)$" (list _ label (app string->number n)))
     (define bi (hash-string label))
     (vector-set! boxes bi (box-replace/add label n (vector-ref boxes bi)))]))

(define (box-remove label ps)
  (remf (lambda (e) (equal? label (car e))) ps))

(define (box-replace/add label n ps)
  (let loop ([ps ps])
    (match ps
      [(cons (cons (== label) _) ps)
       (cons (cons label n) ps)]
      [(cons e ps)
       (cons e (loop ps))]
      [null
       (list (cons label n))])))

(define (focus-power boxes)
  (for/sum ([slots (in-vector boxes)] [bi (in-naturals 1)])
    (for/sum ([lens (in-list slots)] [si (in-naturals 1)])
      (* bi si (cdr lens)))))

;; ----

(module+ test
  (require rackunit)
  (check-equal? (hash-string "HASH") 52)

  (define parts (string-split "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" #rx","))
  (define boxes (run parts))
  (for/list ([bi (in-naturals)] [ps (in-vector boxes)] #:when (pair? ps)) (list bi ps))
  (check-equal? (focus-power boxes) 145))

(module+ main
  (define parts (string-split (string-trim (port->string)) #rx","))
  ;; part 1
  (for/sum ([part (in-list parts)])
    (hash-string part))
  ;; part 2
  (define boxes (run parts))
  (focus-power boxes)
  (void))
