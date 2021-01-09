#lang racket
(require "02-project.rkt")

(require rackunit)
(require rackunit/text-ui)

(define all-tests
  (test-suite
   "all"
   (test-suite
    "private"
    (test-case "zz" (check-equal?
                       (fri (zz 4) null)
                       (zz 4)))
    (test-case "qq" (check-equal?
                       (fri (qq (zz 4) (zz 2)) null)
                       (qq (zz 2) (zz 1))))
    (test-case "true" (check-equal?
                       (fri (true) null)
                       (true)))
    (test-case "false" (check-equal?
                       (fri (false) null)
                       (false)))
    (test-case "seq" (check-equal?
                       (fri (.. (zz 2) (.. (qq (zz 4) (zz 2)) (empty))) null)
                       (.. (zz 2) (.. (qq (zz 2) (zz 1)) (empty)))))
    (test-case "set" (check-equal?
                       (fri (s (set (zz 2) (qq (zz 4) (zz 2)) (true) (false) (true) (qq (zz 2) (zz 1)))) null)
                       (s (set (zz 2) (qq (zz 2) (zz 1)) (true) (false)))))
    (test-case "if" (check-equal?
                       (fri (if-then-else (=? (qq (zz 4) (zz 2)) (qq (zz 2) (zz 1))) (qq (zz 2) (zz 4)) (qq (zz 4) (zz 6))) null)
                       (qq (zz 1) (zz 2))))
    (test-case "is-zz1" (check-equal?
                       (fri (is-zz? (zz 2)) null)
                       (true)))
    (test-case "is-zz2" (check-equal?
                       (fri (is-zz? (empty)) null)
                       (false)))
    (test-case "is-qq1" (check-equal?
                       (fri (is-qq? (qq (zz 4) (zz 2))) null)
                       (true)))
    (test-case "is-qq2" (check-equal?
                       (fri (is-qq? (zz 2)) null)
                       (false)))
    (test-case "is-bool1" (check-equal?
                       (fri (is-bool? (true)) null)
                       (true)))
    (test-case "is-bool2" (check-equal?
                       (fri (is-bool? (zz 2)) null)
                       (false)))
    (test-case "is-seq1" (check-equal?
                       (fri (is-seq? (.. (zz 2) (.. (true) (qq (zz 2) (zz 3))))) null)
                       (true)))
    (test-case "is-seq2" (check-equal?
                       (fri (is-seq? (zz 2)) null)
                       (false)))
    (test-case "is-prop-seq1" (check-equal?
                       (fri (is-proper-seq? (.. (zz 2) (.. (true) (empty)))) null)
                       (true)))
    (test-case "is-prop-seq2" (check-equal?
                       (fri (is-proper-seq? (.. (zz 2) (.. (true) (qq (zz 2) (zz 3))))) null)
                       (false)))
    (test-case "is-empty1" (check-equal?
                       (fri (is-empty? (empty)) null)
                       (true)))
    (test-case "is-empty2" (check-equal?
                       (fri (is-empty? (zz 2)) null)
                       (false)))
    (test-case "is-set1" (check-equal?
                       (fri (is-set? (s (set (zz 2) (qq (zz 4) (zz 2)) (true) (false) (true) (qq (zz 2) (zz 1))))) null)
                       (true)))
    (test-case "is-set2" (check-equal?
                       (fri (is-set? (zz 2)) null)
                       (false)))

    (test-case "add1" (check-equal?
                       (fri (add (=? (zz 1) (zz 1)) (false)) null)
                       (true)))
    
    (test-case "add2" (check-equal?
                       (fri (add (.. (zz 1) (.. (zz 1) (empty))) (.. (false) (empty))) null)
                       (.. (zz 1) (.. (zz 1) (.. (false) (empty))))))

    (test-case "add-vars1" (check-equal?
                       (fri (vars (list "s1" "s2")
                                  (list (s (set (zz 1) (qq (zz 2) (zz 10)) (zz 3))) (s (set (zz 4) (qq (zz 4) (zz 20)))))
                                  (add (valof "s1") (valof "s2"))) null)
                       (s (set (zz 1) (qq (zz 1) (zz 5)) (zz 3) (zz 4)))))

    (test-case "add-mul1" (check-equal?
                       (fri (add (mul (qq (zz 1) (zz 2)) (qq (zz 3) (zz 5))) (qq (zz 2) (zz 10))) null)
                       (qq (zz 1) (zz 2))))
    (test-case "add-mul2" (check-equal?
                       (fri (mul (add (zz 1) (zz 2)) (qq (zz 2) (zz 3))) null)
                       (qq (zz 2) (zz 1))))
    (test-case "add-mul-leq" (check-equal?
                       (fri (add (mul (leq? (zz 2) (qq (zz 10) (zz 3))) (true)) (false)) null)
                       (true)))

    (test-case "leq1" (check-equal?
                       (fri (leq? (true) (false)) null)
                       (false)))
    (test-case "leq2" (check-equal?
                       (fri (leq? (s (set (zz 1) (qq (zz 2) (zz 10)) (zz 3))) (s (set (zz 4) (qq (zz 4) (zz 20))))) null)
                       (false)))
    (test-case "leq3" (check-equal?
                       (fri (leq? (.. (zz 1) (.. (zz 1) (empty))) (.. (false) (empty))) null)
                       (false)))
    
    (test-case "=" (check-equal?
                       (fri (=? (leq? (s (set (zz 1) (qq (zz 2) (zz 10)) (zz 3))) (s (set (zz 4) (qq (zz 4) (zz 20))))) (false)) null)
                       (true)))
    
    (test-case "left1" (check-equal?
                       (fri (left (add (.. (qq (zz 2) (zz 10)) (empty)) (.. (true) (qq (zz 3) (zz 2))))) null)
                       (qq (zz 1) (zz 5))))
    (test-case "right1" (check-equal?
                       (fri (right (add (.. (qq (zz 2) (zz 10)) (empty)) (.. (true) (qq (zz 3) (zz 2))))) null)
                       (.. (true) (qq (zz 3) (zz 2)))))
    (test-case "left2" (check-equal?
                       (fri (left (mul (qq (zz 2) (zz 10)) (qq (zz 5) (zz 3)))) null)
                       (zz 1)))
    (test-case "right2" (check-equal?
                       (fri (right (add (qq (zz 3) (zz 10)) (qq (zz 1) (zz 2)))) null)
                       (zz 5)))
    (test-case "left3" (check-equal?
                       (fri (left (add (s (set (zz 1) (qq (zz 2) (zz 10)) (zz 3))) (s (set (zz 4) (qq (zz 4) (zz 20)))))) null)
                       (zz 1)))
    (test-case "right3" (check-equal?
                       (fri (right (add (s (set (zz 1) (qq (zz 2) (zz 10)) (zz 3))) (s (set (zz 4) (qq (zz 4) (zz 20)))))) null)
                       (s (set (qq (zz 1) (zz 5)) (zz 3) (zz 4)))))

    (test-case "~1" (check-equal?
                       (fri (~ (leq? (s (set (zz 1) (qq (zz 2) (zz 10)) (zz 3))) (s (set (zz 4) (qq (zz 4) (zz 20)))))) null)
                       (true)))
    (test-case "~2" (check-equal?
                       (fri (~ (add (qq (zz 3) (zz 4)) (qq (zz 3) (zz 4)))) null)
                       (qq (zz -3) (zz 2))))
    
    (test-case "mul-vars1" (check-equal?
                       (fri (vars (list "s1" "s2" "s3")
                                  (list (s (set (false) (true))) (s (set (zz 1) (zz 2) (zz 3))) (s (set (zz 4) (zz 5))))
                                  (mul (valof "s1") (mul (valof "s2") (valof "s3")))) null)
                       (s
                        (set
                         (.. (false) (.. (zz 2) (zz 5)))
                         (.. (true) (.. (zz 1) (zz 4)))
                         (.. (false) (.. (zz 3) (zz 5)))
                         (.. (true) (.. (zz 1) (zz 5)))
                         (.. (true) (.. (zz 3) (zz 4)))
                         (.. (false) (.. (zz 2) (zz 4)))
                         (.. (true) (.. (zz 2) (zz 4)))
                         (.. (false) (.. (zz 1) (zz 5)))
                         (.. (false) (.. (zz 1) (zz 4)))
                         (.. (false) (.. (zz 3) (zz 4)))
                         (.. (true) (.. (zz 2) (zz 5)))
                         (.. (true) (.. (zz 3) (zz 5)))))))

    
    
 
    (test-case "all1" (check-equal?
                      (fri (all? (s (set (zz 1) (zz 2) (true)))) null)
                      (true)))

    (test-case "all2" (check-equal?
                       (fri (all? (s (set (zz 1) (false) (true)))) null)
                       (false)))
    (test-case "any1" (check-equal?
                       (fri (any? (.. (zz 1) (.. (false) (true)))) null)
                       (true)))
    (test-case "any2" (check-equal?
                       (fri (any? (.. (false) (.. (false) (empty)))) null)
                       (false)))

    
    (test-case "call-fun-rec" (check-equal?
                            (fri (call (fun "stirling2" (list "n" "k")
                                            (if-then-else (leq? (valof "k") (zz 0))
                                                          (if-then-else (leq? (valof "n") (zz 0)) (zz 1) (zz 0))
                                                          (if-then-else (leq? (valof "n") (zz 0))
                                                                        (zz 0)
                                                                        (add (mul (valof "k") (call (valof "stirling2")
                                                                                                    (list (add (valof "n") (zz -1)) (valof "k"))))
                                                                             (call (valof "stirling2")
                                                                                   (list (add (valof "n") (zz -1)) (add (valof "k") (zz -1))))))))
                                       (list (zz 9) (zz 5))) null)
                            (zz 6951)))

    (test-case "call-proc" (check-equal?
                            (fri (call (proc "p1" (add (zz 1) (zz 2))) null) null)
                            (zz 3)))
    
    (test-case "numerator" (check-equal?
                            (numerator (mul (zz 2) (qq (zz 2) (zz 3))))
                            (zz 4)))
    (test-case "denominator" (check-equal?
                              (denominator (mul (zz 2) (qq (zz 2) (zz 3))))
                              (zz 3)))
    (test-case "inv1" (check-equal?
                       (inv (mul (zz 2) (qq (zz 2) (zz 3))))
                       (qq (zz 3) (zz 4))))
    (test-case "inv2" (check-equal?
                       (inv (.. (true) (.. (true) (.. (false) (empty)))))
                       (.. (false) (.. (true) (.. (true) (empty))))))
    (test-case "map" (check-equal?
                      (mapping (fun "add1" (list "a") (add (valof "a") (zz 1))) (.. (zz 1) (.. (zz 2) (.. (zz 3) (empty)))))
                      (.. (zz 2) (.. (zz 3) (.. (zz 4) (empty))))))
    (test-case "filter" (check-equal?
                         (filtering (fun "leq 3" (list "a") (leq? (valof "a") (zz 3))) (.. (zz 1) (.. (zz 4) (.. (zz 2) (.. (zz 3) (.. (zz 5) (.. (zz 1) (empty))))))))
                         (.. (zz 1) (.. (zz 2) (.. (zz 3) (.. (zz 1) (empty)))))))
    (test-case "fold" (check-equal?
                       (folding (fun "add" (list "a" "b") (add (valof "a") (valof "b"))) (zz 0) (.. (zz 1) (.. (zz 4) (.. (zz 2) (.. (zz 3) (.. (zz 5) (.. (zz 1) (empty))))))))
                       (zz 16)))
    )))
(run-tests all-tests)