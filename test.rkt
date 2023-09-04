#lang play
(print-only-errors true)

(require "T1.rkt")

;P1a): damos un ejemplo de uso de la gramatica
(orp (varp "a") (andp (varp "b") (notp (varp "c"))))

; P1b)
; test definidos en el enunciado
(test (ocurrences (varp "a") "b") 0 )
(test (ocurrences (varp "a") "a") 1 )
(test (ocurrences (andp (varp "a") (varp "a")) "a") 2 )
; tests extras
(test (ocurrences (andp (varp "a") (varp "b")) "a") 1 )
(test (ocurrences  (orp(andp (varp "b") (varp "b")) (notp (varp "b"))) "b" ) 3)
(test (ocurrences  (orp(andp (varp "a") (varp "b")) (notp (varp "b"))) "b" ) 2)
(test (ocurrences  (orp(andp (varp "a") (varp "b")) (notp (varp "b"))) "b" ) 2)

;P1c)
; test definidos en el enunciado
(test (vars (varp "a")) (list "a"))
(test (vars (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (vars (andp (varp "a")(varp "a")) ) (list "a"))
; test extras
(test (vars (andp(orp(andp (varp "a")(varp "a"))(varp "b"))(varp "c"))) (list "a" "b" "c"))
(test (vars (andp(orp(andp (varp "a")(varp "b"))(varp "c"))(varp "d"))) (list "a" "b" "c" "d"))

; P1 d)
; test definidos en el enunciado
(test (all-enviroments (list)) (list (list)))
(test (all-enviroments (list "a")) (list (list (cons "a" #t)) (list (cons "a" #f)))  )
(test (all-enviroments (list "a" "b")) (list (list (cons "a" #t) (cons "b" #t))
                                             (list (cons "a" #t) (cons "b" #f))
                                             (list (cons "a" #f) (cons "b" #t))
                                             (list (cons "a" #f) (cons "b" #f))
                                             )  )
; tests extras
(test (all-enviroments (list "a" "b" "c") ) (list
                                             (list (cons "a" #t) (cons "b" #t) (cons "c" #t))
                                             (list (cons "a" #t) (cons "b" #t) (cons "c" #f))
                                             (list (cons "a" #t) (cons "b" #f) (cons "c" #t))
                                             (list (cons "a" #t) (cons "b" #f) (cons "c" #f))
                                             (list (cons "a" #f) (cons "b" #t) (cons "c" #t))
                                             (list (cons "a" #f) (cons "b" #t) (cons "c" #f))
                                             (list (cons "a" #f) (cons "b" #f) (cons "c" #t))
                                             (list (cons "a" #f) (cons "b" #f) (cons "c" #f))
                                             ) )


; P1 e)
; tst definidos en el enunciado
(test (eval (varp "a") (list (cons "a" #t))) #t )
(test (eval (varp "a") (list (cons "a" #f))) #f )
(test/exn (eval (varp "a") (list)) "eval: variable a is not defined in enviroment"  )
; test extras
(test (eval (andp (orp (varp "a")( varp "b")) (andp (varp "a") (varp "b")))
            (list (cons "a" #t) (cons "b" #f)))#f )

(test/exn (eval (andp (varp "a")(varp "b")) (list (cons "a" #t))) "eval: variable b is not defined in enviroment"  )


; P1 f)
; tests definidos en el enunciado
(test (tautology? (orp (varp "a") (notp(varp "a")))) #t )
(test (tautology? (andp (varp "a") (notp(varp "a")))) #f )
; test extras
(test (tautology? (orp (orp (varp "a") (varp "b")) (varp "c")))  #f )
(test (tautology? (orp (orp (varp "a") (notp (varp "b")))
                       (notp(orp (varp "a") (notp (varp "b")))) ))  #t )


; P2 a)
; tests definidos  en el enunciado
(test (simplify-negations (notp (notp (varp "a"))) ) (varp "a") )
(test (simplify-negations (notp (andp (varp "a") (varp "b"))) ) (orp (notp (varp "a")) (notp (varp "b"))) )
(test (simplify-negations (notp (orp (varp "a")(varp "b"))) ) (andp (notp (varp "a")) (notp (varp "b"))) )
(test (simplify-negations (notp (orp (notp(varp "a")) (varp "b"))) ) (andp (notp(notp (varp "a"))) (notp (varp "b"))) )

; tests extras
(test (simplify-negations (varp "a")) (varp "a"))
(test (simplify-negations (andp (andp  (varp "a") (varp "b"))(orp (varp "c") (varp "d")) ))
      (andp (andp  (varp "a") (varp "b"))(orp (varp "c") (varp "d")) ) )


; P2 b)
; tests definidos en el enucniado
(test (distribute-and (andp (orp (varp "a") (varp "b")) (varp "c"))) (orp (andp (varp "a")(varp "c"))(andp (varp "b")(varp "c"))) )
(test (distribute-and (andp (varp "c")(orp (varp "a")(varp "b")))) (orp (andp (varp "c")(varp "a"))(andp (varp "c")(varp "b"))) )

; test extras
; ; ; ( a or b ) and ( c or  d )-> (a and (c or d)) or (b and (c or d) )
(test (distribute-and (andp (orp (varp "a") (varp "b"))
                                 (orp (varp "c") (varp "d"))))
      (orp
       ( andp (varp "a") (orp (varp "c")( varp "d")))
       ( andp (varp "b") (orp (varp "c")( varp "d")))
       ) )

; ; ; ; ( a or b ) and ( c and  d )-> (a and (c and d ) ) or ( b and (c and d))
(test (distribute-and (andp (orp (varp "a") (varp "b"))
                                 (andp (varp "c") (varp "d"))))
      (orp
       (andp (varp "a") (andp (varp "c")(varp "d")))
       (andp (varp "b") (andp (varp "c")(varp "d")))) )

; ; ; ; ; (a and  b) and ( c or d )-> ((a and b) and c ) or ( (a and b) and d )
(test (distribute-and (andp (andp  (varp "a") (varp "b"))
                                 (orp (varp "c") (varp "d")) ))
      (orp
       (andp (andp (varp "a") (varp "b")) (varp "c"))
       (andp (andp (varp "a")(varp "b")) (varp "d")))
      )


; P2 c )
; tests definidos en el enucniado
(test (( apply-until (lambda (x) (/ x (add1 x)))
                     (lambda (x new-x) (<= (- x new-x) 0.1))) 1) 0.25)
; test extras
(test ((apply-until(lambda (x) (* 2 x)) (lambda (x new-x) (> new-x 15) )) 1) 16)
(test ((apply-until(lambda (x) (add1 x)) (lambda (x new-x) (> (expt 2 new-x) 15) )) 0) 4)

; P2d)
; tests definidos en el enucniado
(test (DNF  (andp (orp (varp "a")(varp "b"))
                  (orp (varp "c")(varp "d"))) )
      (orp (orp (andp (varp "a")(varp "c"))
                (andp (varp "a")(varp "d")))
           (orp (andp (varp "b")(varp "c"))
                (andp (varp "b")(varp "d")))
           ))
; tests extras
(test (DNF (varp "a")) (varp "a") )
(test (DNF (notp(notp (varp "a")))) (varp "a"))
(test (DNF (notp (orp (notp(varp "a")) (varp "b"))) )
      (andp (varp "a") (notp (varp "b"))) )

; not ((not a) and(b or c ))-> ( a  or (not b  and  not c ) )
(test (DNF (notp (andp (notp(varp "a")) (orp (varp "b") (varp "c")))) )
      (orp (varp "a")(andp (notp (varp "b"))(notp (varp "c")))) )



; ------------------P3-------------------------------------------------
; Para esta parte se utilizan los mismos tests que en las partes anteriores
; P3a)

(test (fold-ocurrences (varp "a") "b") 0 )
(test (fold-ocurrences (varp "a") "a") 1 )
(test (fold-ocurrences (andp (varp "a") (varp "a")) "a") 2 )
(test (fold-ocurrences (andp (varp "a") (varp "b")) "a") 1 )
(test (fold-ocurrences  (orp(andp (varp "b") (varp "b")) (notp (varp "b"))) "b" ) 3)
(test (fold-ocurrences  (orp(andp (varp "a") (varp "b")) (notp (varp "b"))) "b" ) 2)
(test (fold-ocurrences  (orp(andp (varp "a") (varp "b")) (notp (varp "b"))) "b" ) 2)

;P3b)
(test (fold-vars (varp "a")) (list "a"))
(test (fold-vars (andp (varp "a") (varp "b"))) (list "a" "b"))
(test (fold-vars (andp (varp "a")(varp "a")) ) (list "a"))
(test (fold-vars (andp(orp(andp (varp "a")(varp "a"))(varp "b"))(varp "c"))) (list "a" "b" "c"))
(test (fold-vars (andp(orp(andp (varp "a")(varp "b"))(varp "c"))(varp "d"))) (list "a" "b" "c" "d"))

; P3c)
(test (fold-simplify-negations (notp (notp (varp "a"))) ) (varp "a") )
(test (fold-simplify-negations (notp (andp (varp "a") (varp "b"))) ) (orp (notp (varp "a")) (notp (varp "b"))) )
(test (fold-simplify-negations (notp (orp (varp "a")(varp "b"))) ) (andp (notp (varp "a")) (notp (varp "b"))) )
(test (fold-simplify-negations (notp (orp (notp(varp "a")) (varp "b"))) ) (andp (notp(notp (varp "a"))) (notp (varp "b"))) )
(test (fold-simplify-negations (varp "a")) (varp "a"))
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

