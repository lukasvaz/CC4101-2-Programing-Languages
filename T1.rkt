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
    [(andp v1 v2) (+ (ocurrences v1 s) (ocurrences v2 s))]
    [(orp v1 v2) (+ (ocurrences v1 s) (ocurrences v2 s))]
    [(notp v1) (ocurrences v1 s)]
    )
  )

;----------P1c)--------------------
; vars :: Prop  -> (Listof String)
; devuelve una lista con los nombres de todas las variables  que ocurren en la proposicion (sin duplicados)
(define (vars prop)
  (match prop
    [(varp value) (list value)]
    [(andp v1 v2) (remove-duplicates (append (vars v1) (vars v2)))]
    [(orp v1 v2) (remove-duplicates (append (vars v1) (vars v2)))]
    [(notp v1) (vars v1)]
    ) )

;----------P1d)--------------------
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

;----------P1e)--------------------
; eval :: Prop (Listof (Pair Strin Boolean))  -> Boolean
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


;----------P1f)--------------------
; tautology? :: Prop -> Boolean
;retorna #t si  la proposicion es una
;  tautologia (ie  es verdadera para cualquier ambiente de evaluacion)
(define (tautology? proposition)
  (match (filter (lambda (i) (not i )) (map  (lambda (i) (eval proposition i))
                                             (all-enviroments (vars proposition))))
    [(cons _ _) #f]
    [(list) #t]
    ))

; --------------------P2)-----------------------
;----------P2a)--------------------
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

;----------P2b)--------------------
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
    
    [(orp v1 v2) (orp (distribute-and v1) (distribute-and v2))]
    
    ; not
    [(notp v) (notp (distribute-and v))]
    )
  )

;(a and b) and (c or d)-> ((a or b) and c) or ( (a or b) and d )
; (p or q) and  (( w and e) or (r and t) or ( y and u))

;----------P2c)--------------------
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


;----------P2d)--------------------
; DNF :: Prop ->Prop
; dada una proposicion le aplica las transformaciones ya definidas  tantas veces como sea necesario
; para lograr  una  forma normal  disyuntiva


(define (DNF prop)
  (define my-equal? (lambda (x x-new) (if (equal? x x-new) #t #f)))
  ((apply-until (lambda (x) (distribute-and (simplify-negations x)))  my-equal?)  prop)
  )

; -------------------------P3----------------------
; P3a)
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

; P3 b)
; usando fold redefinimos:

; fold-ocurrences:: Prop String -> Number
; devuelve la cantidad de veces que una variable aparece en una proposicion
(define (fold-ocurrences prop str)
  ((fold-prop (lambda (value) (if (equal?  value str) 1  0)) + + identity)
   prop)
  )

; fold-vars :: Prop  -> (Listof String)
; devuelve una lista con los nombres de todas las variables  que ocurren en la proposicion (sin duplicados)
(define (fold-vars prop)
  ((fold-prop (lambda (x) (list x))
              (lambda(x1 x2) (remove-duplicates(append x1 x2)))
              (lambda(x1 x2) (remove-duplicates(append x1 x2)))
              (lambda (x) (list x))
              )
   prop)
  )

; fold-simplify-negations :: Prop -> Prop
; simplifica todas las negaciones dobles (ie ~~ p) y aplica las leyes de Morgan
; para distribuir las negaciones (ie ~(p or q)-> ~p and ~q).La funcion se aplica solo una vez


(define (fold-simplify-negations proposition)
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

; distribute-and :: Prop -> Prop
; aplica la regla de distributividad para el and (p and (q or s)-> (p and q) or (p and s ))
; para obtener un  DNF.Esta funcion se aplica solo una vez
(define (fold-distribute-and proposition)

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


