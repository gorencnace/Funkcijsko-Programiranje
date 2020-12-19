#lang racket
(define ones (cons 1 (lambda () ones)))

(define naturals
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))

(define fibs
  (letrec ([f (lambda (x y) (cons x (lambda () (f y (+ x y)))))])
    (f 1 1)))

(define (first n tok)
  (if (= n 0)
      null
      (cons (car tok) (first (- n 1) ((cdr tok))))))

(define (squares tok)
  (letrec ([f (lambda (x) (cons (* (car x) (car x)) (lambda () (f ((cdr x))))))])
    (f tok)))

(define-syntax sml
  (syntax-rules (nil null hd tl ::)
    [(sml nil) null]
    [(sml null x) (null? x)]
    [(sml hd x) (car x)]
    [(sml tl x) (cdr x)]
    [(sml x :: y) (append (list x) y)]))

; zakasnitev
(define (my-delay thunk) 
  (mcons 0 (mcons 0 thunk)))

; sprožitev
(define (my-force prom)
  (if (= 0 (remainder (mcar prom) 5))
      (begin (set-mcar! prom (+ (mcar prom) 1))
             (set-mcar! (mcdr prom) ((mcdr (mcdr prom))))
             (mcar (mcdr prom)))
      (begin (set-mcar! prom (+ (mcar prom) 1))
             (mcar (mcdr prom)))))

(define (partitions k n)
  (letrec ([f (lambda (k n m)
                (cond [(= k 1) (if (> m n) 0 1)]
                      [(> m n) 0]
                      [#t (+ (f k n (+ m 1)) (f (- k 1) (- n m) m))]))])
    (f k n 1)))

