#lang racket

; To je komentar.

#|
To je vecvrsticni
komentar
|#

(define x "Hello world!")

; Operacije
(define q 3)
(define w (+ q 2))
(define e (+ q 2 1 w))

; Funkcije
(define sestej1
  (lambda (a b)
    (+ a b)))

; Sintakticna olepsava funkcije
(define (sestej2 a b)
  (+ a b))

; if stavek
(if (< 2 3) "a" 100)

(define (potenca x n)
  (if (= n 0)
      1
      (* x (potenca x (- n 1)))))

; currying
(define potenca2
  (lambda (x)
    (lambda (n)
      (potenca x n))))

; Seznami in pari
(define p1 (cons "a" 1))

; sestej elemente v seznamu
(define (vsota_sez sez)
  (if (null? sez)
      0
      (+ (car sez) (vsota_sez (cdr sez)))))

; filter
(define (filter1 f sez)
  (if (null? sez)
      null
      (if (f (car sez))
          (cons (car sez) (filter f (cdr sez)))
          ((filter1 f (cdr sez))))))


; vgnezdeno stetje
(define a (list 1 2 5 "a"))
(define b (list (list 1 2 (list #f) "lala") (list 1 2 3) 5))


(define (prestej sez)
  (if (null? sez)
      0
      (if (list? (car sez))
          (+ (prestej (car sez)) (prestej (cdr sez)))
          (+ 1 (prestej (cdr sez))))))

; switch stavek COND
(define (prestej1 sez)
  (cond [(null? sez) 0]
        [(list? (car sez)) (+ (prestej1 (car sez)) (prestej (cdr sez)))]
        [#t (+ 1 (prestej (cdr sez)))]))

; let
(define (test_let a)
  (let ([a 3]
        [b (+ a 2)])
    (+ a b)))
;

; let*
(define (test_let* a)
  (let* ([a 3]
        [b (+ a 2)])
    (+ a b)))


; letrec
(define (test_letrec a)
  (letrec ([b 3]
           [c (lambda (x) (+ a b d x))]
           [d (+ a 1)])
    (c a)))
; > (testt_letrec 50
; 154 ; a=50, b = 3, c = ..., d = 51
; (c 50) = 50 + 3 + 51 + 50 = 154

; letrec narobe
;(define (test_letrec2 a)
;  (letrec ([b 3]
;           [c (+ d 1))]
;           [d (+ a 1)])
;    (+ d a)))
; d : undefined
; zato ker d se nima vrednosti