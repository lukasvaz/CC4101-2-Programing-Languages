#lang play
(print-only-errors true)

(require "T1.rkt")

;P1a): damos un ejemplo de uso de la gramatica
; (orp (varp "a") (andp (varp "b") (notp (varp "c"))))

; P1b)
(test (ocurrences (varp "a") "b") 0 )
(test (ocurrences (varp "a") "a") 1 )
(test (ocurrences (andp (varp "a") (varp "b")) "a") 1 )
(test (ocurrences (andp (varp "a") (varp "a")) "a") 2 )
(test (ocurrences  (orp(andp (varp "b") (varp "b")) (notp (varp "b"))) "b" ) 3)
(test (ocurrences  (orp(andp (varp "a") (varp "b")) (notp (varp "b"))) "b" ) 2)
;P1c)
(test (vars (varp "a")) (list "a"))
(test (vars (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars (andp (varp "a")(varp "a")) ) (list "a"))
(test (vars (andp(orp(andp (varp "a")(varp "a"))(varp "b"))(varp "c"))) (list "a" "b" "c"))

; P1 d)
(test (all-enviroments (list)) (list (list)))
(test (all-enviroments (list "a")) (list (list (cons "a" #t)) (list (cons "a" #f)))  )
(test (all-enviroments (list "a" "b")) (list (list (cons "a" #t) (cons "b" #t))
                                             (list (cons "a" #t) (cons "b" #f))
                                             (list (cons "a" #f) (cons "b" #t))
                                             (list (cons "a" #f) (cons "b" #f))
                                             )  )
; P1 e)


(define prop (andp (orp (varp "a")( varp "b")) (andp (varp "a") (varp "b"))))
(test (eval (varp "a") (list (cons "a" #t))) #t )
(test (eval (varp "a") (list (cons "a" #f))) #f )
(test (eval prop (list (cons "a" #t) (cons "b" #f)))#f )
(test/exn (eval (varp "a") (list)) "eval: variable a is not defined in enviroment"  )

; P1 f)
(test (tautology? (orp (varp "a") (notp(varp "a")))) #t )
(test (tautology? (andp (varp "a") (notp(varp "a")))) #f )


; P2 a)
(test (simplify-negations (notp (notp (varp "a"))) ) (varp "a") )
(test (simplify-negations (notp (andp (varp "a") (varp "b"))) ) (orp (notp (varp "a")) (notp (varp "b"))) )
(test (simplify-negations (notp (orp (varp "a")(varp "b"))) ) (andp (notp (varp "a")) (notp (varp "b"))) )
(test (simplify-negations (notp (orp (notp(varp "a")) (varp "b"))) ) (andp (notp(notp (varp "a"))) (notp (varp "b"))) )
(test (simplify-negations (andp (andp  (varp "a") (varp "b"))(orp (varp "c") (varp "d")) ))
      (andp (andp  (varp "a") (varp "b"))(orp (varp "c") (varp "d")) ) )

; P2 b)
(test (distribute-and (andp (orp (varp "a") (varp "b")) (varp "c"))) (orp (andp (varp "a")(varp "c"))(andp (varp "b")(varp "c"))) )
(test (distribute-and (andp (varp "c")(orp (varp "a")(varp "b")))) (orp (andp (varp "c")(varp "a"))(andp (varp "c")(varp "b"))) )

; ( a or b ) and ( c or  d )-> (a and (c or d)) or (b and (c or d) )    
(test (distribute-and (andp (orp (varp "a") (varp "b"))
                            (orp (varp "c") (varp "d"))))
      (orp
       ( andp (varp "a") (orp (varp "c")( varp "d")))
       ( andp (varp "b") (orp (varp "c")( varp "d")))
       ) )

; ; ( a or b ) and ( c and  d )-> (a and (c and d ) ) or ( b and (c and d))
(test (distribute-and (andp (orp (varp "a") (varp "b"))
                            (andp (varp "c") (varp "d"))))
      (orp
       (andp (varp "a") (andp (varp "c")(varp "d")))
       (andp (varp "b") (andp (varp "c")(varp "d")))) )
; ; (a and  b) and ( c or d )-> ((a and b) and c ) or ( (a and b) and d )
(test (distribute-and (andp (andp  (varp "a") (varp "b"))
                            (orp (varp "c") (varp "d")) ))
      (orp
       (andp (andp (varp "a") (varp "b")) (varp "c"))
       (andp (andp (varp "a")(varp "b")) (varp "d")))
      )

; P2 c )
(test (( apply-until (lambda (x) (/ x (add1 x)))
                     (lambda (x new-x) (<= (- x new-x) 0.1))) 1) 0.25)
; P2d)
; (a or b) and (c or d)

(test (DNF  (andp (orp (varp "a")(varp "b"))
 (orp (varp "c")(varp "d"))) )      
      (orp (orp (andp (varp "a")(varp "c"))
           (andp (varp "a")(varp "d")))
           (orp (andp (varp "b")(varp "c"))
           (andp (varp "b")(varp "d")))
            ))

; P3a)
(test (fold-ocurrences (varp "a") "b")  0 )
(test (fold-ocurrences (varp "a") "a") 1 )
(test (fold-ocurrences (andp (varp "a") (varp "b")) "a") 1 )
(test (fold-ocurrences (andp (varp "a") (varp "a")) "a") 2 )
(test (fold-ocurrences  (orp(andp (varp "b") (varp "b")) (notp (varp "b"))) "b" ) 3)
(test (fold-ocurrences  (orp(andp (varp "a") (varp "b")) (notp (varp "b"))) "b" ) 2)

;P3b)
(test (fold-vars (varp "a")) (list "a"))
(test (fold-vars (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (fold-vars (andp (varp "a")(varp "a")) ) (list "a"))
(test (fold-vars (andp(orp(andp (varp "a")(varp "a"))(varp "b"))(varp "c"))) (list "a" "b" "c"))
; P3c)
(test (fold-simplify-negations (notp (notp (varp "a"))) ) (varp "a") )
(test (fold-simplify-negations (notp (andp (varp "a") (varp "b"))) ) (orp (notp (varp "a")) (notp (varp "b"))) )
(test (fold-simplify-negations (notp (orp (varp "a")(varp "b"))) ) (andp (notp (varp "a")) (notp (varp "b"))) )
(test (fold-simplify-negations (notp (orp (notp(varp "a")) (varp "b"))) ) (andp (notp(notp (varp "a"))) (notp (varp "b"))) )
(test (fold-simplify-negations (andp (andp  (varp "a") (varp "b"))(orp (varp "c") (varp "d")) ))
      (andp (andp  (varp "a") (varp "b"))(orp (varp "c") (varp "d")) ) )

; P3d)

; ------------------------------
(test (fold-distribute-and (andp (orp (varp "a") (varp "b")) (varp "c"))) (orp (andp (varp "a")(varp "c"))(andp (varp "b")(varp "c"))) )
(test (fold-distribute-and (andp (varp "c")(orp (varp "a")(varp "b")))) (orp (andp (varp "c")(varp "a"))(andp (varp "c")(varp "b"))) )

; ; ; ( a or b ) and ( c or  d )-> (a and (c or d)) or (b and (c or d) )    
(test (fold-distribute-and (andp (orp (varp "a") (varp "b"))
                            (orp (varp "c") (varp "d"))))
      (orp
       ( andp (varp "a") (orp (varp "c")( varp "d")))
       ( andp (varp "b") (orp (varp "c")( varp "d")))
       ) )

; ; ; ; ( a or b ) and ( c and  d )-> (a and (c and d ) ) or ( b and (c and d))
(test (fold-distribute-and (andp (orp (varp "a") (varp "b"))
                            (andp (varp "c") (varp "d"))))
      (orp
       (andp (varp "a") (andp (varp "c")(varp "d")))
       (andp (varp "b") (andp (varp "c")(varp "d")))) )

; ; ; ; ; (a and  b) and ( c or d )-> ((a and b) and c ) or ( (a and b) and d )
(test (fold-distribute-and (andp (andp  (varp "a") (varp "b"))
                            (orp (varp "c") (varp "d")) ))
      (orp
       (andp (andp (varp "a") (varp "b")) (varp "c"))
       (andp (andp (varp "a")(varp "b")) (varp "d")))
      )

