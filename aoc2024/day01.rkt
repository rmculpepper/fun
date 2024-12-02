#lang racket

;; read-input : InputPort -> (values (Listof Nat) (Listof Nat))
(define (read-input [in (current-input-port)])
  (define es
    (let loop ()
      (define next (read in))
      (cond [(eof-object? next)
             null]
            [else
             (cons (cons next (read in)) (loop))])))
  (values (map car es) (map cdr es)))

;; augment : (Listof X) -> (Listof (cons X Nat))
;; (define (augment xs)
;;   (for/list ([x (in-list xs)] [i (in-naturals)]) (cons x i)))

(printf "Part 1:\n")
(define-values (as bs) (read-input))
(define sas (sort as <))
(define sbs (sort bs <))
(for/sum ([a (in-list sas)] [b (in-list sbs)])
  (abs (- a b)))

(printf "Part 2:\n")
(define bh (for/fold ([h (hash)]) ([b (in-list bs)])
             (hash-update h b add1 0)))
(for/sum ([a (in-list as)])
  (* a (hash-ref bh a 0)))
