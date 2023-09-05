#lang play

#| P1 |#
#| Parte A |#
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

#| Parte B |#
; ocurrences:: Prop String -> Number
; devuelve la cantidad de veces que una variable aparece en una proposicion
(define (ocurrences prop s)
  (match prop
    [(varp value) (if (equal? value s) 1 0)]
    [(andp v1 v2) (+ (ocurrences v1 s) (ocurrences v2 s))]
    [(orp v1 v2) (+ (ocurrences v1 s) (ocurrences v2 s))]
    [(notp v1) (ocurrences v1 s)]
    )
  )

#| Parte C |#
; vars :: Prop  -> (Listof String)
; devuelve una lista con los nombres de todas las variables  que ocurren en la proposicion (sin duplicados)
(define (vars prop)
  (match prop
    [(varp value) (list value)]
    [(andp v1 v2) (remove-duplicates (append (vars v1) (vars v2)))]
    [(orp v1 v2) (remove-duplicates (append (vars v1) (vars v2)))]
    [(notp v1) (vars v1)]
    ) )


#| Parte C |#
; all-enviroments :: (Listof Strings)  -> (Listof (Listof (Pair String Boolean)))
; dada una lista de variables (sin duplicados) crea todos los ambientes de evaluacion posibles
(define (all-enviroments lista)
  (match lista
    ['() (list (list))]
    [(cons ini  res )
     (append
      (map (lambda (i)
             (append (list (cons ini #t)) i))
           (all-enviroments res))
      (map (lambda (i)
             (append (list (cons ini #f))i))
           (all-enviroments res)))])
  )

#| Parte D |#
;  eval :: Prop (Listof (Pair Strin Boolean))  -> Boolean
;evalua una proposicion p obteniendo los valores de cada varibale  desde un ambiente env
; devolviendo el valor de verdad de  dicha formula
(define (eval proposition lista)
  (match proposition
    [(varp v1) (if (false? (assoc v1 lista)) (error (~a "eval: variable " v1 " is not defined in enviroment"))
                   (cdr(assoc v1 lista)))]
    [(andp v1 v2)(and (eval v1 lista ) (eval v2 lista))]
    [(orp v1 v2)(or (eval v1 lista ) (eval v2 lista))]
    [(notp v1)(not (eval v1 lista ))]
    )
  )

#| Parte D |#
;  tautology? :: Prop -> Boolean
;retorna #t si  la proposicion es una
;  tautologia (ie  es verdadera para cualquier ambiente de evaluacion)
(define (tautology? proposition)
  (match (filter (lambda (i) (not i )) (map  (lambda (i) (eval proposition i))
                                             (all-enviroments (vars proposition))))
    [(cons _ _) #f]
    [(list) #t]
    ))

#| P2 |#

#| Parte A |#
; simplify-negations :: Prop -> Prop
; simplifica todas las negaciones dobles (ie ~~ p) y aplica las leyes de Morgan
; para distribuir las negaciones (ie ~(p or q)-> ~p and ~q).La funcion se aplica solo una vez
(define (simplify-negations proposition)
  (match proposition
    [(varp v) (varp v)]
    [(andp v1 v2)   (andp (simplify-negations v1) (simplify-negations v2))]
    [(orp v1 v2)   (orp (simplify-negations v1) (simplify-negations v2))]
    [(notp v) (match v
                [(varp v) (notp (varp v))]
                [(andp v1 v2) (orp (notp (simplify-negations v1)) (notp (simplify-negations v2)))]
                [(orp v1 v2) (andp (notp (simplify-negations v1))(notp (simplify-negations v2)))]
                [(notp v1) (simplify-negations v1)]
                )]
    )
  )

#| Parte b |#
; distribute-and :: Prop -> Prop
; aplica la regla de distributividad para el and (p and (q or s)-> (p and q) or (p and s ))
; para obtener un  DNF.Esta funcion se aplica solo una vez

(define (distribute-and proposition)
  (match proposition
    ;( or ) and p
    ; caso base
    [(varp v) (varp v)]


    ; ( a or b ) and Prop -> (a and Prop) or (b and Prop )

    [(andp (orp v1 v2) prop)
     (orp
      (andp (distribute-and v1)
            (distribute-and prop ))
      (andp (distribute-and v2)
            (distribute-and prop)))]

    ; p and ( or )
    ; ; Prop and ( c or d )-> (Prop and c ) or ( Prop and d )
    [(andp prop (orp v3 v4))
     (orp (andp (distribute-and prop) (distribute-and v3))
          (andp (distribute-and prop) (distribute-and v4)))]

    [(andp v1 v2) (andp (distribute-and v1) (distribute-and v2))]
    ;and, or externo
    [(orp v1 v2) (orp (distribute-and v1) (distribute-and v2))]
    ; not
    [(notp v) (notp (distribute-and v))]
    )
  )

;(a and b) and (c or d)-> ((a or b) and c) or ( (a or b) and d )
; (p or q) and  (( w and e) or (r and t) or ( y and u))

#| Parte c |#
; apply-until :: (a-> a) ( a a -> Boolean ) -> a -> a
; dada una  funcion f  y  un predicado p retorna una nueva funcion
; que dado un elemento x aplica  f  hasta  que el predicado p  retorna #t
(define (apply-until f p )
  (define (check f p x)  (if (p x (f x)) (f x) (check f p (f x)) ))
  (lambda (x) (check f p x))
  )

; #se define la funcion auxiliar check
; check :: (a-> a) (a a -> Boolean )  a ->  a
; esta funcion toma la misma  funcion y el predicado de apply-until  y un nuevo entero ,con el cual ejecuta
; hasta f hasta que el predicado sea #t y  devuelve  el valor obtenido.Este funcion se retorna como valor en  apply until


#| Parte d |#
; DNF :: Prop ->Prop
; dada una proposicion le aplica las transformaciones ya definidas  tantas veces como sea necesario
; para lograr  una  forma normal  disyuntiva


(define (DNF prop)
  (define my-equal? (lambda (x x-new) (if (equal? x x-new) #t #f)))
  ((apply-until (lambda (x) (distribute-and (simplify-negations x)))  my-equal?)  prop)
  )

#| P3 |#
#| Parte A |#
; fold-prop:: (String -> a) (a a -> a) (a a -> a) (a->a) -> (Prop ->a)
; captura el esquema recursivo de las proposiciones,aplicando
;  las funciones f g  y h   para el caso  variable,operador binario
; y  negacion respectivamente

(define (fold-prop f g h i)
  (lambda (prop)
    (match prop
      [(varp value) (f value)]
      [(andp v1 v2) (g ((fold-prop f g h i ) v1) ((fold-prop f g h i) v2))]
      [(orp v1 v2) (h ((fold-prop f g h i)v1) ((fold-prop f g h i)v2))]
      [(notp v1) (i ((fold-prop f g h i) v1))])))

#| Parte B |#
; usando fold redefinimos:

; ocurrences-2:: Prop String -> Number
; devuelve la cantidad de veces que una variable aparece en una proposicion
(define (ocurrences-2 prop str)
  ((fold-prop (lambda (value) (if (equal?  value str) 1  0)) + + identity)
   prop)
  )

; vars-2 :: Prop  -> (Listof String)
; devuelve una lista con los nombres de todas las variables  que ocurren en la proposicion (sin duplicados)
(define (vars-2 prop)
  ((fold-prop (lambda (x) (list x))
              (lambda(x1 x2) (remove-duplicates(append x1 x2)))
              (lambda(x1 x2) (remove-duplicates(append x1 x2)))
              (lambda (x) (list x))
              )
   prop)
  )
#| Parte C |#
;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean
;evalua una proposicion p obteniendo los valores de cada varibale  desde un ambiente env
; devolviendo el valor de verdad de  dicha formula

(define (eval-2 prop lista) (
                             (fold-prop
                              (lambda (x) (if (false? (assoc x lista))
                                              (error (~a "eval: variable " x " is not defined in enviroment"))
                                              (cdr (assoc x lista) )))
                              (lambda (x1 x2) (and (eval x1 lista ) (eval x2 lista)))
                              (lambda (x1 x2) (or (eval x1 lista ) (eval x2 lista)))
                              (lambda (x) (not (eval x lista )))
                            )prop)
  )

#| Parte D |#
; simplify-negations-2 :: Prop -> Prop
; simplifica todas las negaciones dobles (ie ~~ p) y aplica las leyes de Morgan
; para distribuir las negaciones (ie ~(p or q)-> ~p and ~q).La funcion se aplica solo una vez


(define (simplify-negations-2 proposition)
  ((fold-prop (lambda (x) (varp x))
              (lambda (v1 v2) (andp v1  v2))
              (lambda (v1 v2) (orp v1  v2))
              (lambda (prop)
                (match prop
                  [(varp v) (notp (varp v)) ]
                  [(orp v1 v2) (andp (notp v1)(notp v2))]
                  [(andp v1 v2)(orp (notp v1) (notp v2))]
                  [(notp v) v]
                  ))) proposition)
  )

#| Parte E |#
; distribute-and-2  :: Prop -> Prop
; aplica la regla de distributividad para el and (p and (q or s)-> (p and q) or (p and s ))
; para obtener un  DNF.Esta funcion se aplica solo una vez
(define (distribute-and-2 proposition)

  (( fold-prop (lambda (x) (varp x))
               (lambda (x1 x2)
                 (
                  match (andp x1 x2)
                   [(andp (orp v1 v2) Prop) (orp (andp v1 Prop) (andp v2 Prop))]
                   [(andp Prop (orp v3 v4)) (orp (andp Prop v3) (andp Prop v4))]
                   [(andp Prop (orp v3 v4)) (orp (andp Prop v3) (andp Prop v4))]
                   [_ (andp x1 x2)]
                   ))

               (lambda (x1 x2) (orp x1 x2))
               (lambda (x) (notp x))) proposition)
  )


