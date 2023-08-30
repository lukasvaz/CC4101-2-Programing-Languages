#lang play
; (provide Prop)
; (provide ocurrences)
;----------P1a)--------------------
; gramatica:
;   <Prop>:: (varp <value>)
;            |(andp <var1> <var2>)
;            |(orp <var1> <var2>)
;            |(notp <var1> <var2>)

(deftype Prop
  (varp value)
  (andp var1 var2)
  (orp var1 var2)
  (notp var1))

;----------P1b)--------------------
; ocurrences:: Prop String -> Number
; devuelve la cantidad de veces que una variable aparece en una proposicion
(define (ocurrences prop s)
  (match prop
    [(varp value) (if (equal? value s) 1 0)]
    [(notp v1) (ocurrences v1 s)]
    [(andp v1 v2) (+ (ocurrences v1 s) (ocurrences v2 s))]
    [(orp v1 v2) (+ (ocurrences v1 s) (ocurrences v2 s))]
    )
  )

;----------P1c)--------------------
; vars :: Prop  -> (Listof String)
; devuelve una lista con los nombres de todas las variables  que ocurren en la proposicion (sin duplicados)
(define (vars prop)
  (match prop
    [(varp value) (list value)]
    [(notp v1) (vars v1)]
    [(andp v1 v2) (remove-duplicates (append (vars v1) (vars v2)))]
    [(orp v1 v2) (remove-duplicates (append (vars v1) (vars v2)))]
    ) )

;----------P1d)--------------------
; all-enviroments :: (Listof Strings)  -> (Listof (Listof (Pair String Boolean)))
; dada una lista de variables (sin duplicados) crea todos los ambientes de evaluacion posibles
(define (all-enviroments lista)
  (if (equal? (length lista) 0)
      
      (list (list))

      (map (lambda (e)
            (list  (append (list (cons (car lista) #t)) e)
             (append (list (cons (car lista ) #f)) e)
             ))
           (all-enviroments (cdr lista))
           )

      )
  )


; (equal? (all-enviroments (list "a")) (list
;                                     (list (cons "a" #t))
;                                     (list (cons "a" #f))
;                                     ))

(all-enviroments (list "a"))
