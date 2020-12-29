#lang racket

(define (power a b)
  (if (= b 0)
      1
      (* a (power a (- b 1)))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (fib x)
  (cond [(= x 1) 1]
        [(= x 2) 1]
        [#t (+ (fib (- x 1)) (fib (- x 2)))]))

(define (reverse sez)
  (if (null? sez)
      null
      (append (reverse (cdr sez)) (list (car sez)))))

(define (remove el sez)
  (if (null? sez)
      null
      (if (= (car sez) el)
          (remove el (cdr sez))
          (append (list (car sez)) (remove el (cdr sez))))))

(define (map f sez)
  (if (null? sez)
      null
      (list* (f (car sez)) (map f (cdr sez)))))

(define (filter f sez)
  (if (null? sez)
      null
      (if (f (car sez)) (list* (car sez) (filter f (cdr sez))) (filter f (cdr sez)))))

(define (zip sez1 sez2)
  (if (or (null? sez1) (null? sez2))
      null
      (list* (cons (car sez1) (car sez2)) (zip (cdr sez1) (cdr sez2)))))

(define (range start end step)
  (if (>= start end)
      (list end)
      (list* start (range (+ start step) end step))))

(define (is-palindrome sez)
  (if (null? sez)
      #t
      (and (= (car sez) (car (reverse sez))) (if (null? (cdr sez)) #t (is-palindrome (cdr (reverse (cdr sez))))))))
  