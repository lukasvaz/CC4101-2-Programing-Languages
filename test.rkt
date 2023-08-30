#lang play
(print-only-errors true)

(require "T1.rkt")

;P1a): damos un ejemplo de uso de la gramatica
(orp (varp "a") (andp (varp "b") (notp (varp "c"))))

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