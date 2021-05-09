#lang racket

(define-syntax and2
  (λ (x)
    (syntax-case x ()
      [(_ x y)
       (syntax (if x y #f))])))

(define-syntax when
  (λ (x)
    (syntax-case x ()
      [(_ e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) #f))])))

(define-syntax unless
  (λ (x)
    (syntax-case x ()
      [(_ e0 e1 e2 ...) (syntax (when (not e0) e1 e2 ...))])))

(let ([begin list])
  (when #t (write "win ") (newline)))

(let ([not (λ (x ) x)] [when 'newer])
  (unless #f (write "win") (newline)))

(let-syntax ([when (λ (x)
                     (syntax-case x ()
                       [(_ e0 e1 e2 ...)
                        (syntax (if e0 (begin e1 e2 ...) #f))]))])
  (when #t (write 'hi) (newline) 'mon!))

(letrec-syntax ((and (λ (x)
                       (syntax-case x ()
                         ((_) (syntax #t))
                         ((_ e) (syntax e))
                         ((_ e1 e2 e3 ...)
                          (syntax (if e1 (and e2 e3 ...) #f)))))))
  (let ([x '(a b c d)])
    (and (pair? x) (pair? (cddr x)) (pair? (cdddr x)) (cadddr x))))
#;
(define-syntax or
  (λ (x)
    (syntax-case x ()
      [(_) (syntax #f)]
      [(_ e) (syntax e)]
      [(_ e1 e2 e3 ...)
       (syntax (let ([t e1]) (if t t (or e2 e3 ...))))])))

(let ([t "okay"])
  (or #f t))

((λ (g0001)
   ((λ (g0002)
      (if g0002 g0002 g0001))
    #f))
 "okay")

(define-syntax let1
  (λ (x)
    (syntax-case x ()
      [(_ ((i v) ...) e1 e2 ...)
       (syntax ((λ (i ...) e1 e2 ...) v ...))])))

(define-syntax let2
  (λ (x)
    (syntax-case x ()
      [(_ ((i v) ...) e1 e2 ...)
       (syntax ((λ (i ...) e1 e2 ...) v ...))]
      [(_ name ((i v) ...) e1 e2 ...)
       (syntax ((letrec ([name (λ (i ...) e1 e2 ...)]) name)
                v ...))])))

(define-syntax let3
  (λ (x)
    (syntax-case x ()
      [(_ name ((i v) ...) e1 e2 ...)
       (identifier? (syntax name))
       (syntax ((letrec ([name (λ (i ...) e1 e2 ...)]) name)
                v ...))]
      [(_ ((i v) ...) e1 e2 ...)
       (syntax ((λ (i ...) e1 e2 ...) v ...))])))

(define-syntax let4
  (letrec ([all-ids?
            (λ (ls)
              (or (null? ls)
                  (and (identifier? (car ls))
                       (all-ids? (cdr ls)))))])
    (λ (x)
      (syntax-case x ()
        [(_ ((i v) ...) e1 e2 ...)
         (all-ids? (syntax (i ...)))
         (syntax ((λ (i ...) e1 e2 ...) v ...))]
        [(_ name ((i v) ...) e1 e2 ...)
         (all-ids? (syntax (name i ...)))
         (syntax ((letrec ([name (λ (i ...) e1 e2 ...)]) name)
                  v ...))]))))

(define-syntax let5
  (λ (x)
    (define all-ids?
      (λ (ls)
        (or (null? ls)
            (and (identifier? (car ls))
                 (all-ids? (cdr ls))))))
    (syntax-case x ()
      [(_ ((i v) ...) e1 e2 ...)
       (if (all-ids? (syntax (i ...)))
           (syntax ((λ (i ...) e1 e2 ...) v ...))
           (raise-syntax-error x "non-identifier found"))]
      [(_ name ((i v) ...) e1 e2 ...)
       (identifier? (syntax name))
       (if (all-ids? (syntax (i ...)))
           (syntax ((letrect ((name (λ (i ...) e1 e2 ...))) name)
                    v ...))
           (raise-syntax-error x "non-identifier found"))])))

(define-syntax let6
  (λ (x)
    (define unique-ids?
      (λ (ls)
        (and (let all-ids? ([ls ls])
               (or (null? ls)
                   (and (identifier? (car ls))
                        (all-ids? (cdr ls)))))
             (let unique? ([ls ls])
               (or (null? ls)
                   (and (let notmem? ([x (car ls)] [ls (cdr ls)])
                          (or (null? ls)
                              (and (not (bound-identifier=? x (car ls)))
                                   (notmem? x (cdr ls)))))
                        (unique? (cdr ls))))))))
    (syntax-case x ()
      [(_ ((i v) ...) e1 e2 ...)
       (if (unique-ids? (syntax (i ...)))
           (syntax ((λ (i ...) e1 e2 ...) v ...))
           (raise-syntax-error "non-identifier or duplicate identifier found"))]
      [(_ name ((i v) ...) e1 e2 ...)
       (identifier? (syntax name))
       (if (unique-ids? (syntax (i ...)))
           (syntax ((letrec ([name (λ (i ...) e1 e2 ...)]) name)
                    v ...))
           (raise-syntax-error "non-identifier or duplicate identifier found"))])))


(let-syntax ([f (λ (y) (syntax (quote y)))])
  (f))

(define-syntax with-syntax
  (λ (x)
    (syntax-case x ()
      [(_ ((p e0) ...) e1 e2 ...)
       (syntax (syntax-case (list e0 ...) ()
                 [(p ...) (begin e1 e2 ...)]))])))
#;
(define-syntax or
  (λ (x)
    (syntax-case x ()
      [(_) (syntax #f)]
      [(_ e) (syntax e)]
      [(_ e1 e2 e3 ...)
       (with-syntax ([rest (syntax (or e2 e3 ...))])
         (syntax (let ([t e1]) (if t t rest))))])))

(define-syntax do
  (λ (orig-x)
    (syntax-case orig-x ()
      [(_ ((var init . step) ...) (e0 e1 ...) c ...)
       (with-syntax ([(step ...)
                      (map (λ (v s)
                             (syntax-case s ()
                               [() v]
                               [(e) (syntax e)]
                               [_ (raise-syntax-error orig-x)]))
                           (syntax (var ...))
                           (syntax (step ...)))])
         (syntax-case (syntax (e1 ...)) ()
           [() (syntax (let doloop ((var init) ...)
                         (if (not e0)
                             (begin c ... (doloop step ...)))))]
           [(e1 e2 ...)
            (syntax (let doloop ([var init] ...)
                      (if e0
                          (begin e1 e2 ...)
                          (begin c ... (doloop step ...)))))]))])))

(define-syntax letrec
  (λ (x)
    (syntax-case x ()
      [(_ ((i v) ...) e1 e2 ...)
       (with-syntax ([(t ...) (generate-temporaries (syntax (i ...)))])
         (syntax (let ([i #f] ...)
                   (let ([t v] ...)
                     (set! i t) ...
                     e1 e2 ...))))])))

(define-syntax cond1
  (λ (x)
    (syntax-case x (else =>)
      [(_ (else e1 e2 ...))
       (syntax (begin e1 e2 ...))]
      [(_ (e0))
       (syntax (let ([t e0]) (if t t)))]
      [(_ (e0) c1 c2 ...)
       (syntax (let ([t e0]) (if t t (cond1 c1 c2 ...))))]
      [(_ (e0 => e1)) (syntax (let ([t e0]) (if t (e1 t))))]
      [(_ (e0 => e1) c1 c2 ...)
       (syntax (let ([t e0]) (if t (e1 t) (cond1 c1 c2 ...))))]
      [(_ (e0 e1 e2 ...)) (syntax (if e0 (begin e1 e2 ...)))]
      [(_ (e0 e1 e2 ...) c1 c2 ...)
       (syntax (if e0 (begin e1 e2 ...) (cond1 c1 c2 ...)))])))

(define-syntax cond2
  (λ (x)
    (syntax-case x ()
      [(_ (x e1 e2 ...))
       (and (identifier? (syntax x))
            (free-identifier=? (syntax x) (syntax else)))
       (syntax (begin e1 e2 ...))]
      [(_ (e0))
       (syntax (let ([t e0]) (if t t)))]
      [(_ (e0) c1 c2 ...)
       (syntax (let ([t e0]) (if t t (cond2 c1 c2 ...))))]
      [(_ (e0 x e1))
       (and (identifier? (syntax x))
            (free-identifier=? (syntax x) (syntax =>)))
       (syntax (let ([t e0]) (if t (e1 t))))]
      [(_ (e0 x e1) c1 c2 ...)
       (and (identifier? (syntax x))
            (free-identifier=? (syntax x) (syntax =>)))
       (syntax (let ([t e0]) (if t (e1 t) (cond2 c1 c2 ...))))]
      [(_ (e0 e1 e2 ...)) (syntax (if e0 (begin e1 e2 ...)))]
      [(_ (e0 e1 e2 ...) c1 c2 ...)
       (syntax (if e0 (begin e1 e2 ...) (cond2 c1 c2 ...)))])))

(define-syntax cond3
  (λ (orig-x)
    (let docond ([x orig-x])
      (syntax-case x (else =>)
        [(_ (else e1 e2 ...)) (syntax (begin e1 e2 ...))]
        [(_ (e0))
         (syntax (let ([t e0]) (if t t)))]
        [(_ (e0) c1 c2 ...)
         (with-syntax ([rest (docond (syntax (cond3 c1 c2 ...)))])
           (syntax (let ([t e0]) (if t t rest))))]
        [(_ (e0 => e1))
         (syntax (let ([t e0]) (if t (e1 t))))]
        [(_ (e0 => e1) c1 c2 ...)
         (with-syntax ([rest (docond (syntax (cond3 c1 c2 ...)))])
           (syntax (let ([t e0]) (if t (e1 t) rest))))]
        [(_ (e0 e1 e2 ...)) (syntax (if e0 (begin e1 e2 ...)))]
        [(_ (e0 e1 e2 ...) c1 c2 ...)
         (with-syntax ([rest (docond (syntax (cond3 c1 c2 ...)))])
           (syntax (if e0 (begin e1 e2 ...) rest)))]
        [_ (raise-syntax-error orig-x)]))))

(define-syntax cond4
  (let ([cond-if-tail
         (λ (clauses)
           (syntax-case clauses ()
             [() (syntax ())]
             [(c1 c2 ...) (syntax ((cond4 c1 c2 ...)))]))])
    (λ (x)
      (syntax-case x (else =>)
        [(_ (else e1 e2 ...)) (syntax (begin e1 e2 ...))]
        [(_ (e0) c ...)
         (with-syntax ([tail (cond-if-tail (syntax (c ...)))])
           (syntax (let ([t e0]) (if t t . tail))))]
        [(_ (e0 => e1) c ...)
         (with-syntax ([tail (cond-if-tail (syntax (c ...)))])
           (syntax (let ([t e0]) (if t (e1 t) . tail))))]
        [(_ (e0 e1 e2 ...) c ...)
         (with-syntax ([tail (cond-if-tail (syntax (c ...)))])
           (syntax (if e0 (begin e1 e2 ...) . tail)))]))))

(define-syntax cond5
  (λ (orig-x)
    (syntax-case orig-x ()
      [(_ c0 c1 ...)
       (with-syntax ([tail (syntax-case (syntax (c1 ...)) ()
                             [() (syntax ())]
                             [(c1 c2 ...) (syntax ((cond5 c1 c2 ...)))])])
         (syntax-case (syntax c0) (else =>)
           [(else e1 e2 ...)
            (null? (syntax tail))
            (syntax (begin e1 e2 ...))]
           [(e0)
            (syntax (let ([t e0]) (if t t . tail)))]
           [(e0 => e1)
            (syntax (let ([t e0]) (if t (e1 t) . tail)))]
           [(e0 e1 e2 ...)
            (syntax (if e0 (begin e1 e2 ...) . tail))]
           [_ (raise-syntax-error orig-x)]))])))

(define-syntax cond6
  (λ (orig-x)
    (let docond ([x orig-x])
      (syntax-case x ()
        [(_ c0 c1 ...)
         (with-syntax
             ([tail (syntax-case (syntax (c1 ...)) ()
                      [() (syntax ())]
                      [(c1 c2 ...)
                       (with-syntax ([e (docond (syntax (cond6 c1 c2 ...)))])
                         (syntax (e)))])])
           (syntax-case (syntax c0) (else =>)
             [(else e1 e2 ...)
              (null? (syntax tail))
              (syntax (begin e1 e2 ...))]
             [(e0)
              (syntax (let ([t e0]) (if t t . tail)))]
             [(e0 => e1)
              (syntax (let ([t e0]) (if t (e1 t) . tail)))]
             [(e0 e1 e2 ...)
              (syntax (if e0 (begin e1 e2 ...) . tail))]
             [_ (raise-syntax-error orig-x)]))]))))

(define-syntax cond7
  (λ (x)
    (syntax-case x ()
      [(_ xc1 xc2 ...)
       (with-syntax ([dots (syntax (... ...))])
         (syntax (letrec-syntax
                     ((cond1 (λ (x)
                               (syntax-case x (else =>)
                                 [(_ (else e1 e2 dots))
                                  (syntax (begin e1 e2 dots))]
                                 [(_ (e0 => e1) dots)
                                  (syntax (let ([t e0])
                                            (cond2 t (e1 t) c dots)))]
                                 [(_ (e0 e1 e2 dots) c dots)
                                  (syntax (cond2 e0
                                                 (begin e1 e2 dots)
                                                 c dots))]
                                 [(_ (e0) c dots)
                                  (syntax (let ([t e0])
                                            (cond2 t t c dots)))]
                                 [_ (syntax-error
                                     (syntax (cond7 xc1 xc2 ...)))])))
                      (cond2 (λ (x)
                               (syntax-case x ()
                                 [(_ e0 e1) (syntax (if e0 e1))]
                                 [(_ e0 e1 c1 c2 dots)
                                  (syntax (if e0
                                              e1
                                              (cond1 c1 c2 dots)))]))))
                   (cond1 xc1 xc2 ...))))])))

(let-syntax ([if (λ (x)
                   (syntax-case x ()
                     [(_ e1 e2 e3) (syntax (if e1 e2 e3))]))])
  (if 1 3 5))
#;
(define-syntax syntax-rules
  (λ (x)
    (syntax-case x ()
      [(_ (k ...) (parrern template) ...)
       (syntax (λ (x)
                 (syntax-case x (k ...)
                   [pattern (syntax template)]
                   ...)))])))

(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (let ([t e1]) (if t t (or e2 e3 ...)))]))
#;
(define-syntax syntax-rules
  (λ (x)
    (syntax-case x ()
      [(_ (k ...) ((keyword . pattern) template) ...)
       (with-syntax ([(dummy ...)
                      (generate-temporiaries (syntax (keyword ...)))])
         (syntax (λ (x)
                   (syntax-case x (k ...)
                     [(dummy .pattern) (syntax template)]
                     ...))))])))
#;
(define-syntax extend-syntax
  (λ (x)
    (define expand-clause
      (λ (c)
        (syntax-case c ()
          [(pattern fender template)
           (syntax (pattern fender (syntax template)))]
          [(pattern template)
           (syntax (pattern (syntax template)))])))
    (syntax-case x ()
      [(_ (k0 k1 ...) c ...)
       (with-syntax ([(c ...) (map expand-clause (syntax (c ...)))])
         (syntax (define-syntax k0
                   (λ (x)
                     (syntax-case x (k1 ...) c ...)))))])))
#;
(define-syntax extend-syntax
  (λ (x)
    (define expand-template
      (λ (t)
        (syntax-case t (with)
          [(with ((pattern expression) ...) template)
           (with-syntax ([template (expand-template (syntax template))])
             (syntax (with-syntax ([pattern expression] ...)
                       template)))
           (template (syntax (syntax template)))])))
    (define expand-clause
      (λ (c)
        (syntax-case c ()
          [(pattern fender template)
           (with-syntax ([template (expand-template (syntax template))])
             (syntax (pattern fender template)))]
          [(pattern template)
           (with-syntax ([template (expand-template (syntax template))])
             (syntax (pattern template)))])))
    (syntax-case x ()
      [(_ (k0 k1 ...) c ...)
       (with-syntax ([(c ...) (map expand-caluse (syntax (c ...)))])
         (syntax (define-syntax k0
                   (λ (x)
                     (syntax-case x (k1 ...) c ...)))))])))
#;
(define-syntax define-structure
  (λ (x)
    (define construct-name
      (λ (template-identifier . args)
        (implicit-identifier
         termplate-identifier
         (string->symbol
          (apply string-append
                 (map (λ (x)
                        (if (string? x)
                            x
                            (symbol->string (syntax-object->datum x))))
                      args))))))
    (syntax-case x ()
      [(_ (name id1 ...))
       (syntax (defin-structure (name id1 ...) ()))]
      [(_ (name id1 ...) ((id2 init) ...))
       (with-syntax
           ([constructor (construct-name (syntax name) "make-" (syntax name))]
            [predicate (construct-name (syntax name) (syntax name) "?")]
            [(access ...)
             (map (λ (x) (construct-name x (syntax name) "-" x))
                  (syntax (id1 ... id2 ...)))]
            [(assign ...)
             (map (λ (x)
                    (construct-name x "set-" (syntax name) "-" x "!"))
                  (syntax (id1 ... id2 ...)))]
            [structure-length
             (+ (length (syntax (id1 ... id2 ...))))]
            [(index ...)
             (let f ([i 1] [ids (syntax (id1 ... id2 ...))])
               (if (null? ids)
                   '()
                   (cons i (f (+ i 1) (cdr ids)))))])
         (syntax (begin
                   (define constructor
                     (λ (id1 ...)
                       (let ([id2 init] ...)
                         (vector 'name id1 ... id2 ...))))
                   (define predicate
                     (λ (x)
                       (and (vector? x)
                            (= (vector-length x) structure-length)
                            (eq? (vector-ref x 0) 'name))))
                   (define access
                     (λ (x)
                       (vector-ref x index)))
                   ...
                   (define assign
                     (λ (x update)
                       (vector-set! x index update)))
                   ...)))])))
#;
(define-syntax quasiquote
  (letrec
      ([gen-cons
        (λ (x y)
          (syntax-case x (quote)
            [(quote x)
             (syntax-case y (quote list)
               [(quote y) (syntax (quote (x . y)))]
               [(list y ...) (syntax (list (quote x) y ...))]
               [y (syntax (cons (quote x) y))])
             (x (syntax-case y (quote list)
                  [(quote ()) (syntax (list x))]
                  [(list y ...) (syntax (list x y ...))]
                  [y (syntax (cons x y))]))]))]
       [gen-append
        (λ (x y)
          (syntax-case x (quote list cons)
            [(quote (x1 x2 ...))
             (syntax-case y (quote)
               [(quote y) (syntax (quote (x1 x2 ... . y)))]
               [y (syntax (append (quote (x1 x2 ...) y)))])]
            [(quote ()) y]
            [(list x1 x2 ...)
             (gen-cons (syntax x1) (gen-append (syntax (list x2 ...)) y))]
            [x (syntax-case y (quote list)
                 [(quote ()) (syntax x)]
                 [y (syntax (append x y))])]))]
       [gen-vector
        (λ (x)
          (syntax-case x (quote list)
            [(quote (x ...)) (syntax (quote #(x ...)))]
            [(list x ...) (syntax (vector x ...))]
            [x (syntax (list->vector x))]))]
       [gen
        (λ (p lev)
          (syntax-case p (unquote unquote-splicing quasiquote)
            [(unquote p)
             (if (=lev 0)
                 (syntax p)
                 (gen-cons (syntax (quote unquote))
                           (gen (syntax (p)) (lev 1))))]
             [((unquote-splicing p) . q)
              (if (= lev 0)
                  (gen-append (syntax p) (gen (syntax q) lev))
                  (gen-cons (gen-cons (syntax (quote unquote-splicing))
                                      (gen (syntax p) (lev 1)))
                            (gen (syntax q) lev)))]
            [(quasiquote p)
             (gen-cons (syntax (quote quasiquote))
                       (gen (syntax (p)) (+ lev 1)))]
            [(p . q)
             (gen-cons (gen (syntax p) lev) (gen (syntax q) lev))]
            [#(x ...) (gen-vector (gen (syntax (x ...)) lev))]
            [p (syntax (quote p))]))])
    (λ (x)
      (syntax-case x ()
        [(- e) (gen (syntax e) 0)]))))

(let ([cons '(a . b)])
  `(,cons . ,cons))

(define-syntax be-like-begin
  (λ (x)
    (syntax-case x ()
      [(_ name)
       (syntax (define-syntax name
                 (λ (x)
                   (syntax-case x ()
                     [(_ e0 e1 (... ...))
                      (syntax (begin e0 e1 (... ...)))]))))])))

(define-syntax or2
  (λ (x)
    (syntax-case x ()
      [(_ e1 e2)
       (syntax ((let ([t e1]) (if t t e2))))])))

