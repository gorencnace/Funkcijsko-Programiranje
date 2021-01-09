#lang racket

(require (rename-in racket (numerator qnumerator)
                    (denominator qdenominator)))
(provide false true zz qq .. empty s
         if-then-else
         is-zz? is-qq? is-bool? is-seq? is-proper-seq? is-empty? is-set?
         add mul leq? rounding =? right left ~ all? any?
         vars valof fun closure call proc
         gt? inv numerator denominator filtering folding mapping
         fri)

(struct true () #:transparent)
(struct false () #:transparent)
(struct zz (n) #:transparent)
(struct qq (e1 e2) #:transparent)
(struct .. (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct s (es) #:transparent)


(struct if-then-else (condition e1 e2) #:transparent)
(struct is-zz? (e1) #:transparent)
(struct is-qq? (e1) #:transparent)
(struct is-bool? (e1) #:transparent)
(struct is-seq? (e1) #:transparent)
(struct is-proper-seq? (e1) #:transparent)
(struct is-empty? (e1) #:transparent)
(struct is-set? (e1) #:transparent)


(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct leq? (e1 e2) #:transparent)
(struct rounding (e1) #:transparent)
(struct =? (e1 e2) #:transparent)
(struct left (e1) #:transparent)
(struct right (e1) #:transparent)
(struct ~ (e1) #:transparent)
(struct all? (e1) #:transparent)
(struct any? (e1) #:transparent)

(struct vars (s e1 e2) #:transparent)
(struct valof (s) #:transparent)

(struct fun (name farg body) #:transparent)
(struct proc (name body) #:transparent)
(struct closure (env f) #:transparent)
(struct call (e args) #:transparent)

(struct getenv () #:transparent)

(define (fri e env)
  (letrec ([fri2 (lambda (e env)
                   (cond [(true? e) e]
                         [(false? e) e]
                         [(zz? e) e]
                         [(qq? e)
                          (let ([a (fri (qq-e1 e) env)]
                                [b (fri (qq-e2 e) env)])
                            (qq (zz (qnumerator (/ (zz-n a) (zz-n b)))) (zz (qdenominator (/ (zz-n a) (zz-n b))))))]
                         [(..? e)
                          (let ([a (fri (..-e1 e) env)]
                                [b (fri (..-e2 e) env)])
                            (if (empty? b)
                                (.. a (empty))
                                (.. a b)))]
                         [(empty? e) e]
                         [(s? e)
                          (if (set-empty? (s-es e)) e
                              (let ([v1 (fri (set-first (s-es e)) env)]
                                    [v2 (fri (s (set-rest (s-es e))) env)])
                                (s (set-add (s-es v2) v1))))]
                         [(if-then-else? e)
                          (let ([c (fri (if-then-else-condition e) env)])
                            (if (true? c) (fri (if-then-else-e1 e) env) (fri (if-then-else-e2 e) env)))]
                         [(is-zz?? e)
                          (if (zz? (fri (is-zz?-e1 e) env)) (true) (false))]
                         [(is-qq?? e)
                          (if (qq? (fri (is-qq?-e1 e) env)) (true) (false))]
                         [(is-bool?? e)
                          (if (or (true? (fri (is-bool?-e1 e) env)) (false? (fri (is-bool?-e1 e) env))) (true) (false))]
                         [(is-seq?? e)
                          (if (or (..? (fri (is-seq?-e1 e) env)) (empty? (fri (is-seq?-e1 e) env))) (true) (false))]
                         [(is-proper-seq?? e)
                          (let ([v (fri (is-proper-seq?-e1 e) env)])
                            (cond [(..? v)
                                   (if (empty? (fri (..-e2 v) env)) (true) (fri (is-proper-seq? (fri (..-e2 v) env)) env))]
                                  [(empty? v) (true)]
                                  [#t (false)]))]
                         [(is-empty?? e)
                          (if (empty? (fri (is-empty?-e1 e) env)) (true) (false))]
                         [(is-set?? e)
                          (if (and (s? (is-set?-e1 e)) (set? (s-es (is-set?-e1 e)))) (true) (false))]
                         [(add? e)
                          (let ([v1 (fri (add-e1 e) env)]
                                [v2 (fri (add-e2 e) env)])
                            (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                                   (if (or (true? v1) (true? v2))
                                       (true)
                                       (false))]
                                  [(and (zz? v1) (zz? v2))
                                   (fri (zz (+ (zz-n v1) (zz-n v2))) env)]
                                  [(and (qq? v1) (qq? v2))
                                   (let ([n1 (fri (qq-e1 v1) env)]
                                         [d1 (fri (qq-e2 v1) env)]
                                         [n2 (fri (qq-e1 v2) env)]
                                         [d2 (fri (qq-e2 v2) env)])
                                     (fri (qq (zz (+ (* (zz-n n1) (zz-n d2)) (* (zz-n n2) (zz-n d1)))) (zz (* (zz-n d1) (zz-n d2)))) env))]
                                  [(and (qq? v1) (zz? v2))
                                   (let ([n (fri (qq-e1 v1) env)]
                                         [d (fri (qq-e2 v1) env)])
                                     (fri (qq (zz (+ (zz-n n) (* (zz-n v2) (zz-n d)))) d) env))]
                                  [(and (zz? v1) (qq? v2))
                                   (let ([n (fri (qq-e1 v2) env)]
                                         [d (fri (qq-e2 v2) env)])
                                     (fri (qq (zz (+ (zz-n n) (* (zz-n v1) (zz-n d)))) d) env))]
                                  [(empty? v1) v2]
                                  [(and (true? (fri (is-proper-seq? v1) env)) (..? v2))
                                   (let ([a (fri (..-e1 v1) env)]
                                         [b (fri (..-e2 v1) env)])
                                     (if (empty? b)
                                         (.. a v2)
                                         (.. a (fri (add b v2) env))))]
                                  [(and (true? (fri (is-proper-seq? v1) env)) (empty? v2)) v1]
                                  [(and (s? v1) (s? v2))
                                   (s (set-union (s-es v1) (s-es v2)))]
                                  ))]
                         [(mul? e)
                          (let ([v1 (fri (mul-e1 e) env)]
                                [v2 (fri (mul-e2 e) env)])
                            (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                                   (if (and (true? v1) (true? v2))
                                       (true)
                                       (false))]
                                  [(and (zz? v1) (zz? v2))
                                   (zz (* (zz-n v1) (zz-n v2)))]
                                  [(and (qq? v1) (qq? v2))
                                   (let ([n1 (fri (qq-e1 v1) env)]
                                         [d1 (fri (qq-e2 v1) env)]
                                         [n2 (fri (qq-e1 v2) env)]
                                         [d2 (fri (qq-e2 v2) env)])
                                     (fri (qq (zz (* (zz-n n1) (zz-n n2))) (zz (* (zz-n d1) (zz-n d2)))) env))]
                                  [(and (qq? v1) (zz? v2))
                                   (let ([n (fri (qq-e1 v1) env)]
                                         [d (fri (qq-e2 v1) env)])
                                     (fri (qq (zz (* (zz-n v2) (zz-n n))) d) env))]
                                  [(and (zz? v1) (qq? v2))
                                   (let ([n (fri (qq-e1 v2) env)]
                                         [d (fri (qq-e2 v2) env)])
                                     (fri (qq (zz (* (zz-n v1) (zz-n n))) d) env))]
                                  [(and (s? v1) (s? v2))
                                   (let ([mn1 (s-es v1)]
                                         [mn2 (s-es v2)])
                                     (if (or (set-empty? mn1) (set-empty? mn2))
                                         (s (set))
                                         (s (set-add
                                             (set-union (s-es (fri (mul (s mn1) (s (set-rest mn2))) env))
                                                        (s-es (fri (mul (s (set-rest mn1)) (s mn2)) env)))
                                             (fri (.. (set-first mn1) (set-first mn2)) env)))))]                                   
                                  [#t (error "mnozenca nista enakega tipa")]))]
                         [(leq?? e)
                          (let ([v1 (fri (leq?-e1 e) env)]
                                [v2 (fri (leq?-e2 e) env)])
                            (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                                   (if (or (false? v1) (true? v2))
                                       (true)
                                       (false))]
                                  [(and (zz? v1) (zz? v2))
                                   (if (or (< (zz-n v1) (zz-n v2)) (= (zz-n v1) (zz-n v2)))
                                       (true)
                                       (false))]
                                  [(and (qq? v1) (qq? v2))
                                   (let ([n1 (fri (qq-e1 v1) env)]
                                         [d1 (fri (qq-e2 v1) env)]
                                         [n2 (fri (qq-e1 v2) env)]
                                         [d2 (fri (qq-e2 v2) env)])
                                     (if (or (< (/ (zz-n n1) (zz-n d1)) (/ (zz-n n2) (zz-n d2))) (= (/ (zz-n n1) (zz-n d1)) (/ (zz-n n2) (zz-n d2))))
                                         (true)
                                         (false)))]
                                  [(and (qq? v1) (zz? v2))
                                   (let ([n (fri (qq-e1 v1) env)]
                                         [d (fri (qq-e2 v1) env)])
                                     (if (or (< (/ (zz-n n) (zz-n d)) (zz-n v2)) (= (/ (zz-n n) (zz-n d)) (zz-n v2)))
                                         (true)
                                         (false)))]
                                  [(and (zz? v1) (qq? v2))
                                   (let ([n (fri (qq-e1 v2) env)]
                                         [d (fri (qq-e2 v2) env)])
                                     (if (or (< (zz-n v1) (/ (zz-n n) (zz-n d))) (= (zz-n v1) (/ (zz-n n) (zz-n d))))
                                         (true)
                                         (false)))]
                                  [(and (..? v1) (..? v2))
                                   (let ([a1 (fri (..-e1 v1) env)]
                                         [b1 (fri (..-e2 v1) env)]
                                         [a2 (fri (..-e1 v2) env)]
                                         [b2 (fri (..-e2 v2) env)])
                                     (cond [(not (..? b1)) (true)]
                                           [(not (..? b2)) (false)]
                                           [#t (fri (leq? b1 b2) env)]))]
                                  [(and (s? v1) (s? v2))
                                   (if (subset? (s-es v1) (s-es v2))
                                       (true)
                                       (false))]
                                  [#t (error "nista enakega tipa")]
                                  ))]
                         [(rounding? e)
                          (let ([v (fri (rounding-e1 e) env)])
                            (cond [(zz? v) v]
                                  [(qq? v)
                                   (let ([z1 (fri (qq-e1 v) env)]
                                         [z2 (fri (qq-e2 v) env)])
                                     (fri (zz (round (/ (zz-n z1) (zz-n z2)))) env))]
                                  [#t (error "Vnesite celo ali racionalno stevilo")]))]
                         [(=?? e)
                          (let ([v1 (fri (=?-e1 e) env)]
                                [v2 (fri (=?-e2 e) env)])
                            (cond [(and (true? (fri (is-bool? v1) env)) (true? (fri (is-bool? v2) env)))
                                   (if (or (and (true? v1) (true? v2)) (and (false? v1) (false? v2))) (true) (false))]
                                  [(and (zz? v1) (zz? v2))
                                   (if (= (zz-n v1) (zz-n v2)) (true) (false))]
                                  [(and (qq? v1) (qq? v2))
                                   (let ([n1 (fri (qq-e1 v1) env)]
                                         [d1 (fri (qq-e2 v1) env)]
                                         [n2 (fri (qq-e1 v2) env)]
                                         [d2 (fri (qq-e2 v2) env)])
                                     (if (and (= (zz-n n1) (zz-n n2)) (= (zz-n d1) (zz-n d2))) (true) (false)))]
                                  [(and (..? v1) (..? v2))
                                   (let ([a1 (fri (..-e1 v1) env)]
                                         [b1 (fri (..-e2 v1) env)]
                                         [a2 (fri (..-e1 v2) env)]
                                         [b2 (fri (..-e2 v2) env)])
                                     (if (true? (fri (=? a1 a2) env))
                                         (fri (=? b1 b2) env)
                                         (false)))]
                                  [(and (empty? v1) (empty? v2))
                                   (true)]
                                  [(and (s? v1) (s? v2))
                                   (if (set=? (s-es v1) (s-es v2)) (true) (false))]
                                  [#t (false)]
                                  ))]
                         [(left? e)
                          (let ([v (fri (left-e1 e) env)])
                            (cond [(qq? v) (fri (qq-e1 v) env)]
                                  [(..? v) (fri (..-e1 v) env)]
                                  [(s? v) (fri (set-first (s-es v)) env)]
                                  ))]
                         [(right? e)
                          (let ([v (fri (right-e1 e) env)])
                            (cond [(qq? v) (fri (qq-e2 v) env)]
                                  [(..? v) (fri (..-e2 v) env)]
                                  [(s? v) (fri (s (set-rest (s-es v))) env)]
                                  ))]
                         [(~? e)
                          (let ([v (fri (~-e1 e) env)])
                            (cond [(true? v) (false)]
                                  [(false? v) (true)]
                                  [(zz? v) (zz (- (zz-n v)))]
                                  [(qq? v)
                                   (let ([n (qq-e1 v)]
                                         [d (qq-e2 v)])
                                     (fri (qq (zz (- (zz-n n))) d) env))]
                                  [#t (error "sintaksa izraza ni pravilna")]))]
                         [(all?? e)
                          (let ([v (fri (all?-e1 e) env)])
                            (cond [(empty? v) (true)]
                                  [(..? v)
                                   (if (false? (fri (..-e1 v) env)) (false) (fri (all? (..-e2 v)) env))]
                                  [(s? v)
                                   (if (set-empty? (s-es v))
                                       (true)
                                       (if (false? (fri (set-first (s-es v)) env)) (false) (fri (all? (s (set-rest (s-es v)))) env)))]
                                  [#t (if (false? v) (false) (true))]))]
                         [(any?? e)
                          (let ([v (fri (any?-e1 e) env)])
                            (cond [(empty? v) (false)]
                                  [(..? v)
                                   (if (not (false? (fri (..-e1 v) env))) (true) (fri (any? (..-e2 v)) env))]
                                  [(s? v)
                                   (if (set-empty? (s-es v))
                                       (false)
                                       (if (not (false? (fri (set-first (s-es v)) env))) (true) (fri (any? (s (set-rest (s-es v)))) env)))]
                                  [#t (if (not (false? v)) (true) (false))]))]
                         [(vars? e)
                          (let ([v1 (vars-s e)]
                                [v2 (vars-e1 e)]
                                [v3 (vars-e2 e)])
                            (if (and (list? v1) (list? v2))
                                (fri v3 (if (null? env) (map cons v1 (map (lambda (x) (fri x env)) v2)) (append (map cons v1 (map (lambda (x) (fri x env)) v2)) env)))
                                (fri v3 (if (null? env) (list (cons v1 (fri v2 env))) (append (list (cons v1 (fri v2 env))) env)))))]
                         [(valof? e)
                          (let ([v (cdr (assoc (valof-s e) env))])
                            (if (closure? v) v (fri v env)))]
                         [(fun? e) (closure env e)]
                         [(proc? e) (closure env e)]
                         [(call? e)
                          (let ([o (fri (call-e e) env)]
                                [a (call-args e)])
                            (if (closure? o)
                                (if (fun? (closure-f o))
                                    (fri (vars (append (fun-farg (closure-f o)) (list (fun-name (closure-f o))))
                                               (append a (list (closure-f o)))
                                               (fun-body (closure-f o)))
                                         env)
                                    (fri (vars (proc-name (closure-f o)) (closure-f o) (proc-body (closure-f o))) env))
                                (error "klic funkcije nima ustreznih argumentov"))
                            )]



                         [(getenv? e) env]
                         [#t (error "sintaksa izraza ni pravilna")]
                         ))])
    (fri2 e env)))

(define (numerator e1)
  (fri (left e1) null))

(define (denominator e1)
  (fri (right e1) null))

(define (gt? e1 e2)
  (fri (mul (leq? e2 e1) (~ (and (leq? e2 e1) (leq? e1 e2)))) null))

(define (inv e1)
  (let ([v (fri e1 null)])
    (cond [(qq? v) (fri (qq (right v) (left v)) null)]
          [(zz? v) (fri (qq (zz 1) v) null)]
          [(..? v) (fri (call (fun "obrat" (list "l")
                                   (if-then-else (is-empty? (right (valof "l")))
                                                 (valof "l")
                                                 (add (call (valof "obrat") (list (right (valof "l"))))
                                                      (.. (left (valof "l")) (empty)))))
                              (list v))
                        null)])))

(define (mapping f seq)
  (fri (call (fun "map" (list "l")
                  (if-then-else (is-empty? (right (valof "l")))
                                (.. (call f (list (left (valof "l")))) (empty))
                                (add (.. (call f (list (left (valof "l")))) (empty)) (call (valof "map") (list (right (valof "l")))))))
             (list (fri seq null)))
       null))
                  
  
(define (filtering f seq)
  (fri (call (fun "filter" (list "l")
                  (if-then-else (is-empty? (valof "l"))
                                (empty)
                                (if-then-else (call f (list (left (valof "l"))))
                                              (add (.. (left (valof "l")) (empty)) (call (valof "filter") (list (right (valof "l")))))
                                              (call (valof "filter") (list (right (valof "l")))))))
             (list (fri seq null)))
       null))

   
(define (folding f init seq)
  (fri (call (fun "fold" (list "init" "l")
                  (if-then-else (is-empty? (right (valof "l")))
                                (call f (list (left (valof "l")) (valof "init")))
                                (call (valof "fold") (list  (call f (list (left (valof "l")) (valof "init"))) (right (valof "l"))))))
             (list (fri init null) (fri seq null)))
       null))

;(require racket/trace)
;(trace fri)
; get environment
;(fri (call
;      (fun "kva" (list "a" "b")
;           (getenv))
;      (list (zz 10) (zz 2))) null)

;(inv (.. (zz 1) (.. (zz 2) (empty))))
;(mapping (fun "add a" (list "a") (add (valof "a") (zz 1))) (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty)))))
;(filtering (fun "leq 3" (list "a") (leq? (valof "a") (zz 3))) (.. (zz 1) (.. (zz 4) (.. (zz 2) (.. (zz 3) (.. (zz 5) (.. (zz 1) (empty))))))))
;(folding (fun "add" (list "a" "b") (add (valof "a") (valof "b"))) (zz 0) (.. (zz 1) (.. (zz 4) (.. (zz 2) (.. (zz 3) (.. (zz 5) (.. (zz 1) (empty))))))))