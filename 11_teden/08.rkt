#lang racket

(struct zz (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct leq? (e1 e2) #:transparent)
(struct ~ (e1) #:transparent)
(struct is-zz? (e) #:transparent)
(struct if-then-else (condition e1 e2) #:transparent)

(define (fri e)
  (cond [(zz? e) e]
        [(true? e) e]
        [(false? e) e]
        [(add? e)
         (let ([v1 (fri (add-e1 e))]
               [v2 (fri (add-e2 e))])
           (cond [(and (zz? v1) (zz? v2))
                  (zz (+ (zz-n v1) (zz-n v2)))]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (or (true? v1) (true? v2))
                      (true)
                      (false))]
                 [#t (error "sestevance nista enakega tipa")]))]
        [(mul? e)
         (let ([v1 (fri (mul-e1 e))]
               [v2 (fri (mul-e2 e))])
           (cond [(and (zz? v1) (zz? v2))
                  (zz (* (zz-n v1) (zz-n v2)))]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (and (true? v1) (true? v2))
                      (true)
                      (false))]
                 [#t (error "mnozenca nista enakega tipa")]))]
        [(leq?? e)
         (let ([v1 (fri (leq?-e1 e))]
               [v2 (fri (leq?-e2 e))])
           (cond [(and (zz? v1) (zz? v2))
                  (if (or (< (zz-n v1) (zz-n v2)) (= (zz-n v1) (zz-n v2)))
                      (true)
                      (false))]
                 [(and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
                  (if (or (false? v1) (true? v2))
                      (true)
                      (false))]
                 [#t (error "nista enakega tipa")]))]
        [(~? e)
         (let ([v (fri (~-e1 e))])
           (cond [(zz? v) (zz (- (zz-n v)))]
                 [(true? v) (false)]
                 [(false? v) (true)]
                 [#t (error "sintaksa izraza ni pravilna")]))]
        [(is-zz?? e)
         (if (zz? (is-zz?-e e)) (true) (false))]
        [(if-then-else? e)
         (let ([c (fri (if-then-else-condition e))])
           (if (true? c) (fri (if-then-else-e1 e)) (fri (if-then-else-e2 e))))]
        [#t (error "sintaksa izraza ni pravilna")]
        ))

(define-syntax ifte
  (syntax-rules (then else)
    [(ifte cond then e1 else e2)
     (if-then-else cond e1 e2)]))

(define-syntax geq?
  (syntax-rules ()
    [(geq? e1 e2)
     (leq? e2 e1)]))