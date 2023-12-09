#lang racket

(define (parse)
  (define lrs (parse-lrs (read-line)))
  (define line2 (read-line))
  (define h (parse-lines (port->lines)))
  (values lrs h))

(define (parse-lrs line)
  (map (lambda (c)
         (case c
           [(#\L) car]
           [(#\R) cdr]
           [else (error 'parse-lrs "bad char: ~e" c)]))
       (string->list (string-trim line))))

(define (parse-lines lines)
  (define h (make-hasheq))
  (for ([line (in-list lines)])
    (parse-line h line))
  h)

(define (parse-line h line)
  (match (regexp-match #rx"^([0-9A-Z]+) = [(]([0-9A-Z]+), ([0-9A-Z]+)[)]$" line)
    [(list _ node left right)
     (let ([node (string->symbol node)]
           [left (string->symbol left)]
           [right (string->symbol right)])
       (hash-set! h node (cons left right)))]))

(define (symbol-start-node? sym)
  (hash-ref! start-node-table sym
             (lambda () (regexp-match? #rx"A$" (symbol->string sym)))))
(define start-node-table (make-hasheq))

(define (symbol-end-node? sym)
  (hash-ref! end-node-table sym
             (lambda () (regexp-match? #rx"Z$" (symbol->string sym)))))
(define end-node-table (make-hasheq))

;; ----

;; follow : Hash List -> Nat
(define (follow h lrs0)
  (let loop ([lrs lrs0] [start 'AAA] [steps 0])
    ;; (eprintf "node = ~s, ~s\n" start steps)
    (define startp (hash-ref h start #f))
    (cond [(eq? start 'ZZZ) steps]
          [(pair? lrs)
           (define next ((car lrs) startp))
           (loop (cdr lrs) next (add1 steps))]
          [(null? lrs)
           (loop lrs0 start steps)])))

;; ----

(define (follow* h lrs0)
  (define cyclen (length lrs0))
  (define start-nodes (filter symbol-start-node? (hash-keys h)))
  (define end-nodes (filter symbol-end-node? (hash-keys h)))
  (eprintf "start-nodes = ~s\n" start-nodes)
  (eprintf "end-nodes = ~s\n" end-nodes)
  (define (end-node? v) (memq v end-nodes))
  (define (step nodes lr)
    (for/list ([node (in-list nodes)])
      (lr (hash-ref h node))))
  (let loop ([lrs lrs0] [nodes start-nodes] [steps 0])
    (define end-count (count end-node? nodes))
    (when (> end-count 0)
      (printf "~s; nodes = ~s, ~s/~s ~a\n" steps nodes end-count (length nodes)
              (if (zero? (remainder steps cyclen)) 'complete 'partial)))
    (cond [(andmap end-node? nodes)
           steps]
          #;[(> steps 18000) (error 'stop)]
          [(null? lrs)
           (loop lrs0 nodes steps)]
          [else
           (loop (cdr lrs) (step nodes (car lrs)) (add1 steps))])))

;; hh : Hasheq[Symbol => Symbol], one complete LR cycle

(define (make-hh h lrs)
  (define hh (make-hasheq))
  (for ([start (in-hash-keys h)])
    (hash-set! hh start
               (for/fold ([node start]) ([lr (in-list lrs)])
                 (lr (hash-ref h node)))))
  hh)

(define (follow** h lrs0)
  (define hh (make-hh h lrs0))
  (define iter-steps (length lrs0))
  (define start-nodes (filter symbol-start-node? (hash-keys h)))
  (define end-nodes (filter symbol-end-node? (hash-keys h)))
  (define (end-node? v) (memq v end-nodes))
  (define (step nodes)
    (for/list ([node (in-list nodes)])
      (hash-ref hh node)))
  (let loop ([nodes start-nodes] [steps 0])
    (printf "~s; nodes = ~s, ~s/~s\n" steps nodes (count end-node? nodes) (length nodes))
    (cond [(andmap end-node? nodes)
           steps]
          [else
           (loop (step nodes) (+ iter-steps steps))])))

;; h3 : Hasheq[Symbol => (cons Nat Symbol)], maps symbol to next end symbol, number of cycles to get there

(define (fill-h3! h3 hh start)
  (unless (hash-has-key? h3 start)
    (let loop ([node (hash-ref hh start)] [n 1])
      (cond [(symbol-end-node? node) (hash-set! h3 start (cons n node))]
            [else (loop (hash-ref hh node) (add1 n))]))))

(define (follow3 h lrs)
  (define start-nodes (filter symbol-start-node? (hash-keys h)))
  (define hh (make-hh h lrs))
  (define h3 (make-hasheq))
  (define (prep node) (fill-h3! h3 hh node))
  (define (get-next node) (prep node) (hash-ref h3 node))

  (for ([node (filter symbol-end-node? (hash-keys h))])
    (define steps+next (get-next node))
    (eprintf "~s => ~s, ~s\n" node (car steps+next) (cdr steps+next)))

  ;; State = (cons Nat Symbol)
  (define (state-end? s) (zero? (car s)))
  (define (state-n s) (car s))
  (define ((state-dec n) s) (cons (- (car s) n) (cdr s)))
  (define (state-next s) (get-next (cdr s)))

  (define seen (make-hash))
  (define (loop acc states)
    (eprintf "~s, ~s\n" acc states)
    (cond [(andmap state-end? states) acc]
          [(ormap state-end? states)
           #;
           (cond [(hash-ref seen states #f)
                  (error 'cycle)]
                 [else (hash-set! seen states #t)])
           (loop acc 
                 (for/list ([s (in-list states)])
                   (if (state-end? s) (state-next s) s)))]
          [else
           (define n (apply min (map state-n states)))
           (loop (+ acc n) (map (state-dec n) states))]))

  (loop 0 (map get-next start-nodes)))

;; ----

(module+ test
  (require rackunit)
  (let ()
    (define-values (lrs h)
      (with-input-from-file "input/ex08a.txt"
        (lambda () (parse))))
    (check-equal? (follow h lrs) 2))
  (let ()
    (define-values (lrs h)
      (with-input-from-file "input/ex08b.txt"
        (lambda () (parse))))
    (check-equal? (follow h lrs) 6))
  (let ()
    (define-values (lrs h)
      (with-input-from-file "input/ex08c.txt"
        (lambda () (parse))))
    (check-equal? (follow* h lrs) 6))
  (void))

(module+ main
  (define-values (lrs h)
    (with-input-from-file "input/input08.txt"
      (lambda () (parse))))
  (printf "cycle = ~s\n" (length lrs))
  ;; part 1
  ;; (follow h lrs)
  ;; part 2
  #;(follow* h lrs)
  #;(follow** h lrs)
  (follow3 h lrs)
  (void))

#|
cycle = 263
CLZ => 47, CLZ
GKZ => 71, GKZ
JTZ => 61, JTZ
FTZ => 79, FTZ
ZZZ => 67, ZZZ
VCZ => 73, VCZ

The answer in big steps (1 big step = 263 L/R steps)
happens to be the LCM of the cycle lengths of each of the end nodes.
|#
