#lang racket

(define original-graph (make-hash))

(define (parse-graph lines)
  (define graph (make-hash))
  (define start #f) ;; mutated
  (for ([line (in-list lines)] [li (in-naturals)])
    (for ([c (in-string line)] [ci (in-naturals)])
      (define here (cons li ci))
      (hash-set! original-graph here c)
      (define (node ends)
        (graph-add! graph here ends))
      (case c
        [(#\|) (node '(N S))]
        [(#\-) (node '(E W))]
        [(#\L) (node '(N E))]
        [(#\J) (node '(N W))]
        [(#\7) (node '(W S))]
        [(#\F) (node '(E S))]
        [(#\S) (set! start (cons li ci))])))
  (values start graph))

(define (graph-out graph node)
  (hash-ref graph node null))

(define (graph-in graph node)
  (define neighbors
    (for/list ([dir '(N S E W)])
      (node-neighbor node dir)))
  (for/list ([nnode (in-list neighbors)]
             #:when (member node (graph-out graph nnode)))
    nnode))

(define (infer-start-shape graph start)
  (define in-nodes (graph-in graph start))
  (cond [(= (length in-nodes) 2)
         (for ([node in-nodes]) (hash-push! graph start node))
         #t]
        [else
         #f]))

(define (hash-push! h k v)
  (hash-update! h k (lambda (vs) (cons v vs)) null))

(define (graph-add! graph here ends)
  (for ([end (in-list ends)])
    (hash-push! graph here (node-neighbor here end))))

(define (node-neighbor node end)
  (match-define (cons li ci) node)
  (case end
    [(N) (cons (sub1 li) ci)]
    [(S) (cons (add1 li) ci)]
    [(E) (cons li (add1 ci))]
    [(W) (cons li (sub1 ci))]))

(define (graph-next graph here [prev #f])
  (for/first ([next (in-list (hash-ref graph here null))]
              #:when (not (equal? next prev)))
    next))

;; clean-graph! : Graph ->  Void
(define (clean-graph! graph)
  (for ([(node neighbors) (in-hash graph)])
    (define (real-neighbor? nnode) (member node (graph-out graph nnode)))
    (hash-set! graph node (filter real-neighbor? neighbors))))

;; move-class : Node Node -> (U 'up 'down 'horz)
(define (move-class n1 n2)
  (cond [(< (car n1) (car n2)) 'down]
        [(> (car n1) (car n2)) 'up]
        [else 'horz]))

;; cyclen : Graph Node -> Nat or #f
(define (cyclen graph start [h #f])
  (define next (graph-next graph start))
  (let loop ([here (graph-next graph start)] [prev start] [steps 1])
    (when (and h here) (hash-set! h here (move-class prev here)))
    (cond [(equal? here start) steps]
          [here (loop (graph-next graph here prev) here (add1 steps))]
          [else #f])))

;; cycle-set : Graph Node -> NodeSet
(define (cycle-set graph start)
  (define h (make-hash))
  (and (cyclen graph start h) h))

;; count-in-loop : Graph Node -> Nat
(define (count-in-loop graph start)
  (define cycle (cycle-set graph start))
  (define interior (graph-interior graph cycle))
  ;; (remove-subcycles interior)
  (hash-count interior))

;; make-interior? : Graph Cycle -> (Node -> Boolean)
(define (make-interior? graph cycle)
  (define interior (make-hash))
  (define-values (LINES COLS) (graph-dim graph))
  (for ([li (in-range LINES)])
    (define out? #t)
    (define lastv #f)
    (define (flip) (set! out? (not out?)))
    (for ([ci (in-range COLS)])
      (cond [(hash-ref cycle (cons li ci) #f)
             (case (hash-ref original-graph (cons li ci))
               [(#\|) (flip)]
               [(#\-) (void)]
               [(#\L) (set! lastv 'up)]
               [(#\J) (when (eq? lastv 'down) (flip))]
               [(#\7) (when (eq? lastv 'up) (flip))]
               [(#\F) (set! lastv 'down)])]
            [else (unless out? (hash-set! interior (cons li ci) #t))])))
  (lambda (node) (hash-ref interior node #f)))
#;
(define (make-interior? graph cycle)
  (define interior (make-hash))
  (define-values (LINES COLS) (graph-dim graph))
  (for ([li (in-range LINES)])
    (define out? #t)
    (for ([ci (in-range COLS)])
      (case (hash-ref cycle (cons li ci) #f)
        [(vert) (set! out? (not out?))]
        [(horz) (void)]
        [else (unless out? (hash-set! interior (cons li ci) #t))])))
  (lambda (node) (hash-ref interior node #f)))
#;
(define (OLD-make-interior? graph cycle)
  (define interior (make-hash))
  (define-values (LINES COLS) (graph-dim graph))
  (for ([li (in-range LINES)])
    (define out? #t)
    (define border? #f)
    (for ([ci (in-range COLS)])
      (cond [(hash-ref cycle (cons li ci) #f)
             ;; On the loop
             (cond [border?  ;; ... was before too
                    (void)]
                   [else     ;; ... wasn't before
                    (set! out? (not out?))])
             (set! border? #t)]
            [else
             (set! border? #f)
             (unless out?
               (hash-set! interior (cons li ci) #t))])))
  (lambda (node) (hash-ref interior node #f)))

;; graph-interior : Graph NodeSet -> Graph
;; Returns nodes in interior of cycle.
(define (graph-interior graph cycle)
  (define interior? (make-interior? graph cycle))
  (define interior-graph (make-hash))
  (for ([(node neighbors) (in-hash graph)]
        #:when (interior? node))
    (hash-set! interior-graph node (filter interior? neighbors)))
  interior-graph)

;; remove-subcycles : Graph -> Void
(define (remove-subcycles graph)
  (for ([node (in-list (hash-keys graph))])
    (when (hash-ref graph node #f)
      (define cycle (cycle-set graph node))
      (when cycle
        (define interior (graph-interior graph cycle))
        (graph-remove! graph cycle)
        (graph-remove! graph interior)
        (clean-graph! graph)))))

(define (graph-remove! graph nodes)
  (for ([node (in-hash-keys nodes)])
    (hash-remove! graph node)))

(define (graph-dim graph)
  (for/fold ([lines 0] [cols 0] #:result (values (add1 lines) (add1 cols)))
            ([node (in-hash-keys graph)])
    (match-define (cons li ci) node)
    (values (max lines li) (max cols ci))))

;; ----

(module+ main
  (define lines (port->lines))
  (define-values (start graph) (parse-graph lines))
  (unless (infer-start-shape graph start)
    (error 'unknown-start-shape))
  (clean-graph! graph)
  ;; part 1
  (quotient (cyclen graph start) 2)
  ;; part 2
  (count-in-loop graph start)
  (void))
