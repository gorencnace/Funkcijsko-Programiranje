#lang racket

; 0 = Racket
; X = JAIS

(struct konst (int) #:transparent)
(struct negiraj (e) #:transparent)
(struct sestej (e1 e2) #:transparent)
(struct bool (b) #:transparent)
(struct ce-potem-sicer (pogoj res nires) #:transparent)

(struct shrani (vrednost izraz) #:transparent)
(struct brei () #:transparent)


(define (jais2 e)
 (letrec ([jais (lambda (e env)
                  (cond [(konst? e) e]
                        [(bool? e) e]
                        [(sestej? e) (let ([v1 (jais (sestej-e1 e))]
                                           [v2 (jais (sestej-e2 e))])
                                       (if (and (konst? v2) (konst? v2))
                                           (konst (+ (konst-int v1)
                                                     (konst-int v2)))
                                           (error "sestevanec ni stevilka")))]
                        [(negiraj? e) (let ([v (jais (negiraj-e e))])
                                        (cond [(konst? v) (konst (- (konst-int v)))]
                                              [(bool? v) (bool (not (bool-b v)))]
                                              [#t (error "negacija nepricakovanega izraza!")]))]
                        [(ce-potem-sicer? e) (let ([v-test (jais (ce-potem-sicer-pogoj e))])
                                               (if (bool? v-test)
                                                   (if (bool-b v-test)
                                                       (jais (ce-potem-sicer-res e))
                                                       (jais (ce-potem-sicer-nires e)))
                                                   (error "pogoj ni logicna vrednost!")))]
                        [#t (error "sintaksa izraza ni pravilna")]
                        ))])))