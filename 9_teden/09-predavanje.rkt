#lang racket

(define (potenca x n)
  (if (= n 0)
      1
      (* x (potenca x (- n 1)))))

(define (moj_if pogoj res nires)
  (if pogoj res nires))

(define (potenca_moj x n)
  (moj_if (= n 0)
          1
          (* x (potenca_moj x (- n 1)))))
; ne dela, ker if ni vgrajena funkcija ampak nasa funkcija, ki se evaluira takoj ob klicu
;  popravimo :

(define (moj_if2 pogoj res nires) ; sedaj podamo dve funkciji, prej smo podali dva izraza
  (if pogoj (res) (nires)))

(define (potenca_moj2 x n)
  (moj_if2 (= n 0)
          (lambda () 1)
          (lambda () (* x (potenca_moj2 x (- n 1))))))

(define (dolga_operacija x)
  (begin
    (printf "izvajam dolgo operacijo!~n")
    (sleep 1)
    x))

(define (potenca3 x klic_n)
  (cond [(= x 0) 0]
        [(= x 1) 1]
        [(= (klic_n) 1) x]
        [#t (* x (potenca3 x (lambda () (- (klic_n) 1))))]))

; (potenca3 200 (lambda () (dolga_operacija 4))) -> dolga operacija se klice 4krat :(

; uporabimo lokalno okolje
; (potenca3 2 (let ([rez (dolga_operacija 4)]) (lambda () rez))) -> dolga operacija se klice samo enkrat



; TOKOVI
(define enke (cons 1 (lambda () enke)))

(define naravna
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))

(define plusminus
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x -1)))))])
    (f 1)))

(define potence2
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (f 1)))

(define (izpisi n tok)
  (if (> n 1)
      (begin
        (displayln (car tok))
        (izpisi (- n 1) ((cdr tok))))
      (displayln (car tok))))


(define (izppog tok pogoj)
  (cond [(pogoj (car tok)) (begin
                             (displayln (car tok))
                             (izppog ((cdr tok)) pogoj))]
        [#t #t]))




