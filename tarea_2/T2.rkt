#lang play
(print-only-errors)
;; PARTE 1A, 1B, 1F

#|
  <Expr> ::= (num <value>)
            |(tt)
            |(ff)
            (id <sym>)
            |(add  <Expr> <Expr>)
            |(sub  <Expr> <Expr>)
            |(mul  <Expr> <Expr>)
            |(leq  <Expr> <Expr>)
            |(ifc  <Expr> <Expr> <Expr>)
            |(fun   ListOf(<sym>)  <Expr>)
            |(tupl  Listof(<expr>))
            |(proj  <Expr> <Expr>)
            |(app   <id>  <Expr>)
|#

(deftype Expr
  ;; core
  (num n)
  (tt)
  (ff)
  (id x)
  (add l r)
  (sub l r)
  (mul l r)
  (leq l r)
  (ifc prop then els)
  (fun  args body)
  (tupl exprs)
  (proj expr-1 expr-2)
  (app f e)
  )

;; parse :: s-expr -> Expr
;Dada  una s-expr (sintaxis concreta ) genera una
; Expr (sintaxis abstracta).Las operaciones que soporta el lenguaje son: +,-,*, <=, ifc, true ,false,funciones.

(define (parse s-expr)
  (match s-expr
    [n #:when  (number? n) (num n)]
    [x #:when (and (and (not (equal? x 'true)) (not (equal? x 'false)))  (symbol? x)) (id x)]
    ['true  (tt)]
    ['false  (ff)]
    [(list '+ l r) (add  (parse l) (parse r))]
    [(list '- l r) (sub  (parse l) (parse r))]
    [(list '* l r) (mul  (parse l) (parse r))]
    [(list '<= l r ) (leq (parse l) (parse r) )]
    [(list 'if prop then els) (ifc (parse prop) (parse then) (parse els))]
    [(list 'fun  (list elems ... ) e )  (fun  elems (parse e) )]
    [(list 'tuple elems ...) (tupl (map  parse elems))]
    [(list 'proj expr-1 expr-2) (proj (parse expr-1) (parse expr-2))]
    [(list f e ...)  ( app (parse f) (map parse e) )]
    )

  )



;; PARTE 1C, 1G
#|
  <Val> ::= (numV <number>)
            |(boolV <Boolean>)
            |(clousureV <sym> <Expr> <Env>)|
|#

(deftype Val
  (numV n)
  (boolV b)
  (tupleV values)
  (clousureV  id body env)
  )

;; ambiente de sustitución diferida
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; interface ADT (abstract data type) del ambiente
(define empty-env (mtEnv))

;; "Simplemente" asigna un nuevo identificador para aEnv
;(define extend-env aEnv)
;
;; es lo mismo que definir extend-env así:
;; (concepto técnico 'eta expansion')
(define (extend-env id val env) (aEnv id val env))

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x) val (env-lookup x rest))]))

;; PARTE 1D

;; num2num-op :: (Number Number > Number) -> (Val Val -> Val)
; toma  una funcion  que tomea dos numeros  y retorna un numero y retorna  una función  con la misma
; funcionalidad pero  que toma Val  en lugar de  number
(define (num2num-op fun)
  (lambda (x1 x2)
    (match x1
      [(numV n) (let ([op1 n])
                  (match x2
                    [(numV n) (let ([op2 n]) (numV (fun op1 op2)))]
                    [_ (error "num-op:invalid operands")]
                    )
                  )]

      [_  (error "num-op:invalid operands")]
      )
    )
  )
;; num2bool-op :: (Number Number > Number) -> (Val Val -> Val)
; toma  una funcion  que toma dos numeros  y retorna un booleano y retorna  una función  con la misma
; funcionalidad pero  que toma Val  en lugar de  number
(define (num2bool-op fun)
  (lambda (x1 x2)
    (match x1
      [(numV n) (let ([op1 n])
                  (match x2
                    [(numV n) (let ([op2 n]) (boolV (fun op1 op2)))]
                    [_ (error "num-op:invalid operands")]
                    )
                  )]

      [_  (error "num-op:invalid operands")]
      )
    )
  )

;; PARTE 1E, 1G

;------------ funciones auxiliares-------------------
; extend-recursive :: Listof(<sym>) Listof(<Expr>) Env->Env
; dada  una lista de argumentos , sus respectivos  y  un ambiente valores retorna  el ambiente extendido  con
; los  valores de los  argumentos.
  
(define (extend-recursive list-args list-values  fun-env )
  (match list-args
    ['() fun-env]
    [(cons  arg rest-args ) (extend-recursive
                             rest-args
                             (cdr list-values)
                             (extend-env arg (car list-values) fun-env)
                             )]))


; revisar!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; ; get-idx :: Listoof(Expr) Number>Env
; ; dada  una lista de expresiones y  un  entero,retorna la expresion  en tal  indice

;   (define (get-idx lis idx)
;     (if (equal? idx 0) (car lis) (get-idx (cdr lis) (- idx 1)))
;     )

;; eval :: Expr Env -> Val
; evalua  una expresión  (Expr) para cierto ambiente (Env) y retorna  su valor (Val)
(define (eval expr env)

  (match expr
    [(num n) (numV n)]
    [(tt) (boolV #t)]
    [(ff) (boolV #f)]
    [(id x) (env-lookup  x env) ]
    [(add l r)((num2num-op +) (eval l env) (eval r env))]
    [(sub l r)((num2num-op -) (eval l env) (eval r env))]
    [(mul l r)((num2num-op *) (eval l env) (eval r env))]
    [(leq l r)((num2bool-op <=) (eval l env) (eval r env))]

    [(ifc prop then els)
     (let  ([p (eval prop env)])
       (match p
         [(boolV #t) (eval then env)]
         [(boolV #f) (eval els env)]
         ))]
    [(fun id body) (clousureV id body env)]
    [(tupl  exprs) (tupleV (map  (lambda (x) (eval x env)) exprs)   )  ] 
    [(proj (tupl args)  (num  idx))  (eval (list-ref args idx) env ) ]   

    [(app f e)
     (def (clousureV the-arg the-body the-claus-env) (eval f env))
     (def eval-e (map (lambda (x)(eval x  env )) e))
     (def the-ext-env  (extend-recursive the-arg eval-e the-claus-env))
     (eval the-body the-ext-env)
     ]
    )
  )

;; PARTE 2A
;; swaap ::(a b -> c) -> (b a -> c)
;recibe una funcion  que recibe  dos  argumentos  y retorna  una  nueva  funcion  que recibe
; los argumentos  en oden contrario

(define swap*
  (clousureV
   (list 'f)
   (fun '(a b) (app (id 'f) (list (id 'b) (id 'a))))
   empty-env)
  )
;; curry :(:a  b -> c) -> (b-> a -> c)
;recibe una funcion  de tipo  a b->c y retorna  su veresion currificda (a->b->c)

(define curry*
  (clousureV
   (list 'f)
   (fun '(a)   (fun '(b) (app (id 'f) (list (id 'a) (id 'b) ))))
   empty-env
   )
  )

;; uncurry : (b-> a -> c)->(:a  b -> c)
;recibe una funcion  de tipo  (a-> b->c) y retorna  su version descurrificda (a,b->c)
(define uncurry*
  (clousureV
   (list 'f)
   (fun '(a b) (app (app (id 'f) (list (id 'a))) (list (id 'b))) )
   empty-env
   ))


;; partial: (a b -> c) a ->(b -> c)
;recibe una funcion  de tipo  (a-> b->c) y un argumento de tipo a y
;  retorna  una  funcion de tipo (b->c)
(define partial*
  (clousureV
   (list 'f 'a)
   (fun '(b) (app (id 'f) (list (id 'a) (id 'b)) ) )
   empty-env
   )
  )
;; PARTE 2B

;; run ::s-expr (Listof(Pair Symbol Val))-> Val
(define (run s-expr global-env)
  ( eval (parse s-expr)
         (extend-recursive (map (lambda (x) (car x )) global-env)
                           (map (lambda (x) (cdr x )) global-env) empty-env)
         )
  )
