#lang racket

;rekurzivna resitev
(define (fib1 x)
  (cond [(= x 1) 1]
        [(= x 2) 1]
        [#t (+ (fib1 (- x 1))
               (fib1 (- x 2)))]))

(define (fib2 x)
  (letrec ([pomozna (lambda (f1 f2 n)
                      (cond [(= n x) (+ f1 f2)]
                            [#t (pomozna f2 (+ f1 f2) (+ n 1))]))])
    (cond [(= x 1) 1]
          [(= x 2) 1]
          [#t (pomozna 1 1 3)])))

(define fib3
  (letrec ([resitve null]
           [pomozna (lambda (x)
                      (let ([ans (assoc x resitve)])        ;poiscemo resitev v seznamu, shranimo v lokalno spremenljivko ans
                        (if ans
                            (cdr ans)
                            (let ([nova (cond [(= x 1) 1]
                                              [(= x 2) 1]
                                              [#t (+ (pomozna (- x 1))
                                                     (pomozna (- x 2)))])])
                              (begin
                                (set! resitve (cons (cons x nova) resitve))
                                (displayln resitve)
                                nova)))))])
    pomozna))


; MAKRI

; moj-if, ki uporablja besedi then in else
(define-syntax moj-if
  (syntax-rules (then else)
    [(moj-if e1 then e2 else e3)
     (if e1 e2 e3)]))

(define-syntax if3
  (syntax-rules (then elif else)
    [(if3 e1 then e2 elif e3 then e4 else e5)
     (if e1 e2 (if e3 e4 e5))]))

(define-syntax prvi
  (syntax-rules ()
    [(prvi e)
     (car e)]))

(define-syntax drugi
  (syntax-rules ()
    [(drugi e)
     (car ((cdr e)))]))

; anotacija spremenljivk
(define-syntax anotiraj
  (syntax-rules ()
    [(anotiraj e s)
     e]))
                 