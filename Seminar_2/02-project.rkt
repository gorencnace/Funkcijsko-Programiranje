#lang racket

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

(struct getenv () #:transparent)

(define (fri e env)
(letrec ([fri2 (lambda (e env)
                 (cond [(true? e) e]
                       [(false? e) e]
                       [(zz? e) e]
                       [(qq? e)
                        (let ([a (fri2 (qq-e1 e) env)]
                              [b (fri2 (qq-e2 e) env)])
                          (qq (zz (numerator (/ (zz-n a) (zz-n b)))) (zz (denominator (/ (zz-n a) (zz-n b))))))]
                       [(..? e)
                        (let ([a (fri2 (..-e1 e) env)]
                              [b (fri2 (..-e2 e) env)])
                          (if (empty? b)
                              (.. a (empty))
                              (.. a b)))]
                       [(empty? e) e]
                       [(s? e)
                        (if (set-empty? (s-es e)) e
                            (let ([v1 (fri2 (set-first (s-es e)) env)]
                                  [v2 (fri2 (s (set-rest (s-es e))) env)])
                              (s (set-add (s-es v2) v1))))]
                       [(if-then-else? e)
                        (let ([c (fri2 (if-then-else-condition e) env)])
                          (if (true? c) (fri2 (if-then-else-e1 e) env) (fri2 (if-then-else-e2 e) env)))]
                       [(is-zz?? e)
                        (if (zz? (fri2 (is-zz?-e1 e) env)) (true) (false))]
                       [(is-qq?? e)
                        (if (qq? (fri2 (is-qq?-e1 e) env)) (true) (false))]
                       [(is-bool?? e)
                        (if (or (true? (fri2 (is-bool?-e1 e) env)) (false? (fri2 (is-bool?-e1 e) env))) (true) (false))]
                       [(is-seq?? e)
                        (if (..? (fri2 (is-seq?-e1 e) env)) (true) (false))]
                       [(is-proper-seq?? e)
                        (let ([v (fri2 (is-proper-seq?-e1 e) env)])
                          (if (..? v)
                              (if (empty? (fri2 (..-e2 v) env)) (true) (fri2 (is-proper-seq? (fri2 (..-e2 v) env)) env))
                              (false)))]
                       [(is-set?? e)
                        (if (set? (is-set?-e1 e)) (true) (false))]
                       [(add? e)
                        (let ([v1 (fri2 (add-e1 e) env)]
                              [v2 (fri2 (add-e2 e) env)])
                          (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                                 (if (or (true? v1) (true? v2))
                                     (true)
                                     (false))]
                                [(and (zz? v1) (zz? v2))
                                 (fri2 (zz (+ (zz-n v1) (zz-n v2))) env)]
                                [(and (qq? v1) (qq? v2))
                                 (let ([n1 (fri2 (qq-e1 v1) env)]
                                       [d1 (fri2 (qq-e2 v1) env)]
                                       [n2 (fri2 (qq-e1 v2) env)]
                                       [d2 (fri2 (qq-e2 v2) env)])
                                   (fri2 (qq (zz (+ (* (zz-n n1) (zz-n d2)) (* (zz-n n2) (zz-n d1)))) (zz (* (zz-n d1) (zz-n d2)))) env))]
                                [(and (qq? v1) (zz? v2))
                                 (let ([n (fri2 (qq-e1 v1) env)]
                                       [d (fri2 (qq-e2 v1) env)])
                                   (fri2 (qq (zz (+ (zz-n n) (* (zz-n v2) (zz-n d)))) d) env))]
                                [(and (zz? v1) (qq? v2))
                                 (let ([n (fri2 (qq-e1 v2) env)]
                                       [d (fri2 (qq-e2 v2) env)])
                                   (fri2 (qq (zz (+ (zz-n n) (* (zz-n v1) (zz-n d)))) d) env))]
                                [(and (true? (fri2 (is-proper-seq? v1) env)) (..? v2))
                                 (let ([a (fri2 (..-e1 v1) env)]
                                       [b (fri2 (..-e2 v1) env)])
                                   (if (empty? b)
                                       (.. a v2)
                                       (.. a (and b v2))))]
                                [(and (s? v1) (s? v2))
                                 (s (set-union (s-es v1) (s-es v2)))]
                                ))]
                       [(mul? e)
                        (let ([v1 (fri2 (mul-e1 e) env)]
                              [v2 (fri2 (mul-e2 e) env)])
                          (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                                 (if (and (true? v1) (true? v2))
                                     (true)
                                     (false))]
                                [(and (zz? v1) (zz? v2))
                                 (zz (* (zz-n v1) (zz-n v2)))]
                                [(and (qq? v1) (qq? v2))
                                 (let ([n1 (fri2 (qq-e1 v1) env)]
                                       [d1 (fri2 (qq-e2 v1) env)]
                                       [n2 (fri2 (qq-e1 v2) env)]
                                       [d2 (fri2 (qq-e2 v2) env)])
                                   (fri2 (qq (zz (* (zz-n n1) (zz-n n2))) (zz (* (zz-n d1) (zz-n d2)))) env))]
                                [(and (qq? v1) (zz? v2))
                                 (let ([n (fri2 (qq-e1 v1) env)]
                                       [d (fri2 (qq-e2 v1) env)])
                                   (fri2 (qq (zz (* (zz-n v2) (zz-n n))) d) env))]
                                [(and (zz? v1) (qq? v2))
                                 (let ([n (fri2 (qq-e1 v2) env)]
                                       [d (fri2 (qq-e2 v2) env)])
                                   (fri2 (qq (zz (* (zz-n v1) (zz-n n))) d) env))]
                                [(and (s? v1) (s? v2))
                                 (let ([mn1 (s-es v1)]
                                       [mn2 (s-es v2)])
                                   (if (or (set-empty? mn1) (set-empty? mn2))
                                       (s (set))
                                       (s (set-add
                                           (set-union (s-es (fri2 (mul (s mn1) (s (set-rest mn2))) env))
                                                      (s-es (fri2 (mul (s (set-rest mn1)) (s mn2)) env)))
                                           (fri2 (.. (set-first mn1) (set-first mn2)) env)))))]                                   
                                [#t (error "mnozenca nista enakega tipa")]))]
                       [(leq?? e)
                        (let ([v1 (fri2 (leq?-e1 e) env)]
                              [v2 (fri2 (leq?-e2 e) env)])
                          (cond [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                                 (if (or (false? v1) (true? v2))
                                     (true)
                                     (false))]
                                [(and (zz? v1) (zz? v2))
                                 (if (or (< (zz-n v1) (zz-n v2)) (= (zz-n v1) (zz-n v2)))
                                     (true)
                                     (false))]
                                [(and (qq? v1) (qq? v2))
                                 (let ([n1 (fri2 (qq-e1 v1) env)]
                                       [d1 (fri2 (qq-e2 v1) env)]
                                       [n2 (fri2 (qq-e1 v2) env)]
                                       [d2 (fri2 (qq-e2 v2) env)])
                                   (if (or (< (/ n1 d1) (/ n2 d2)) (= (/ n1 d1) (/ n2 d2)))
                                       (true)
                                       (false)))]
                                [(and (qq? v1) (zz? v2))
                                 (let ([n (fri2 (qq-e1 v1) env)]
                                       [d (fri2 (qq-e2 v1) env)])
                                   (if (or (< (/ n d) (zz-n v2)) (= (/ n d) (zz-n v2)))
                                       (true)
                                       (false)))]
                                [(and (zz? v1) (qq? v2))
                                 (let ([n (fri2 (qq-e1 v2) env)]
                                       [d (fri2 (qq-e2 v2) env)])
                                   (if (or (< (zz-n v1) (/ n d)) (= (zz-n v1) (/ n d)))
                                       (true)
                                       (false)))]
                                [(and (..? v1) (..? v2))
                                 (let ([a1 (fri2 (..-e1 v1) env)]
                                       [b1 (fri2 (..-e2 v1) env)]
                                       [a2 (fri2 (..-e1 v2) env)]
                                       [b2 (fri2 (..-e2 v2) env)])
                                   (cond [(not (..? b1)) (true)]
                                         [(not (..? b2)) (false)]
                                         [#t (fri2 (leq? b1 b2) env)]))]
                                [(and (s? v1) (s? v2))
                                 (if (subset? (s-es v1) (s-es v2))
                                     (true)
                                     (false))]
                                [#t (error "nista enakega tipa")]))]
                       [(rounding? e)
                        (let ([v (fri2 (rounding-e1 e) env)])
                          (cond [(zz? v) v]
                                [(qq? v)
                                 (let ([z1 (fri2 (qq-e1 v) env)]
                                       [z2 (fri2 (qq-e2 v) env)])
                                   (fri2 (zz (round (/ (zz-n z1) (zz-n z2)))) env))]
                                [#t (error "Vnesite celo ali racionalno stevilo")]))]
                       [(=?? e)
                        (let ([v1 (fri2 (=?-e1 e) env)]
                              [v2 (fri2 (=?-e2 e) env)])
                          (cond [(and (true? (fri2 (is-bool? v1) env)) (true? (fri2 (is-bool? v2) env)))
                                 (if (or (and (true? v1) (true? v2)) (and (false? v1) (false? v2))) (true) (false))]
                                [(and (zz? v1) (zz? v2))
                                 (if (= (zz-n v1) (zz-n v2)) (true) (false))]
                                [(and (qq? v1) (qq? v2))
                                 (let ([n1 (fri2 (qq-e1 v1) env)]
                                       [d1 (fri2 (qq-e2 v1) env)]
                                       [n2 (fri2 (qq-e1 v2) env)]
                                       [d2 (fri2 (qq-e2 v2) env)])
                                   (if (and (= (zz-n n1) (zz-n n2)) (= (zz-n d1) (zz-n d2))) (true) (false)))]
                                [(and (..? v1) (..? v2))
                                 (let ([a1 (fri2 (..-e1 v1) env)]
                                       [b1 (fri2 (..-e2 v1) env)]
                                       [a2 (fri2 (..-e1 v2) env)]
                                       [b2 (fri2 (..-e2 v2) env)])
                                   (if (true? (fri2 (=? a1 a2) env))
                                       (fri2 (=? b1 b2) env)
                                       (false)))]
                                [(and (empty? v1) (empty? v2))
                                 (true)]
                                [(and (s? v1) (s? v2))
                                 (if (set=? (s-es v1) (s-es v2)) (true) (false))]
                                [#t (false)]
                                ))]
                       [(left? e)
                        (let ([v (fri2 (left-e1 e) env)])
                          (cond [(qq? v) (fri2 (qq-e1 v) env)]
                                [(..? v) (fri2 (..-e1 v) env)]
                                [(s? v) (fri2 (set-first (s-es v)) env)]
                                ))]
                       [(right? e)
                        (let ([v (fri2 (left-e1 e) env)])
                          (cond [(qq? v) (fri2 (qq-e2 v) env)]
                                [(..? v) (fri2 (..-e2 v) env)]
                                [(s? v) (fri2 (set-rest (s-es v)) env)]
                                ))]
                       [(~? e)
                        (let ([v (fri2 (~-e1 e) env)])
                          (cond [(true? v) (false)]
                                [(false? v) (true)]
                                [(zz? v) (zz (- (zz-n v)))]
                                [(qq? v)
                                 (let ([n (qq-e1 v)]
                                       [d (qq-e2 v)])
                                   (fri2 (qq (zz (- (zz-n n))) d) env))]
                                [#t (error "sintaksa izraza ni pravilna")]))]
                       [(all?? e)
                        (let ([v (fri2 (all?-e1 e) env)])
                          (cond [(..? v)
                                 (if (false? (fri2 (..-e1 v) env)) (false) (fri2 (all? (..-e2 v)) env))]
                                [(s? v)
                                 (if (false? (fri2 (set-first (s-es v)) env)) (false) (fri2 (all? (set-rest (s-es v))) env))]
                                [#t (if (false? v) (false) (true))]))]
                       [(any?? e)
                        (let ([v (fri2 (any?-e1 e) env)])
                          (cond [(..? v)
                                 (if (not (false? (fri2 (..-e1 v) env))) (true) (fri2 (any? (..-e2 v)) env))]
                                [(s? v)
                                 (if (not (false? (fri2 (set-first (s-es v)) env))) (true) (fri2 (any? (set-rest (s-es v))) env))]
                                [#t (if (not (false? v)) (true) (false))]))]
                       [(vars? e)
                        (let ([v1 (vars-s e)]
                              [v2 (vars-e1 e)]
                              [v3 (vars-e2 e)])
                          (if (and (list? v1) (list? v2))
                              (fri2 v3 (if (null? env) (map cons v1 (map (lambda (x) (fri2 x env)) v2)) (append env (map cons v1 v2))))
                              (fri2 v3 (if (null? env) (list (cons v1 v2)) (append env (list (cons v1 v2)))))))]
                       [(valof? e) (cdr (assoc (valof-s e) env))]
                       [(getenv? e) env]
                       [#t (error "sintaksa izraza ni pravilna")]
                       ))])
  (fri2 e env)))