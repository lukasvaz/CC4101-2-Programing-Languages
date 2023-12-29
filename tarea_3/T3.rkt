#lang play


#|
  Expr  ::= <num>
          | (+ <Expr> <Expr>)
          | (- <Expr> <Expr>)
          | (* <Expr> <Expr>)
          | (tt)
          | (ff)
          | (if <Expr> <Expr> <Expr>)
          | <id>
          | (fun (<id> : <Type>) <Expr>)
          | (<Expr> <Expr>);
|#
(deftype Expr
  ;; core
  (num n)
  (tt)
  (ff)
  (binop op l r)
  (ifc c th el)
  ;; unary first-class functions
  (id x)
  (fun binder binderType body)
  (app callee arg)


  )

#| BEGIN P1 |#

#|
;; Type ::= <numT>    (revisar)
         | <boolT>
         | (<Type> <Type>)

|#
(deftype Type
  (numT)
  (boolT)
  (arrowT t1 t2)
  )

;; parse-type : s-expr -> Type
; toma un s-expr  la parsea y devuelve un Type
(define (parse-type t)
  (match t
    ['Number (numT)]
    ['Boolean (boolT)]
    [(list '-> l r) (arrowT (parse-type l) (parse-type r))]
    ))

;; parse : s-expr -> Expr
(define (parse s)
  (match s
    [n #:when (number? n) (num n)]
    [x #:when (and (symbol? x) (and (not (equal? x 'true)) (not (equal? x 'false)))) (id x)]
    ['true (tt)]
    ['false (ff)]
    [(list '+ l r) (binop '+ (parse l) (parse r))]
    [(list '- l r) (binop '- (parse l) (parse r))]
    [(list '* l r) (binop '* (parse l) (parse r))]
    [(list '<= l r) (binop '<= (parse l) (parse r))]
    [(list 'if c th el) (ifc (parse c) (parse th) (parse el))]
    [(list 'fun (list binder ': type) body) (fun binder (parse-type type) (parse body))]
    [(list callee arg) (app (parse callee) (parse arg))]
    [_ (error 'parse "invalid syntax: ~a" s)]
    ))

;; Implementación de ambientes de tipos
;; (análoga a la de ambientes de valores)

;; TypeEnv ::= ⋅ | <TypeEnv>, <id> : <Type>
(deftype TypeEnv (mtTenv) (aTenv id type env))
(define empty-tenv (mtTenv))
(define extend-tenv aTenv)

(define (tenv-lookup x env)
  (match env
    [(mtTenv) (error 'tenv-lookup "free identifier: ~a" id)]
    [(aTenv id type rest) (if (symbol=? id x) type (tenv-lookup x rest))]
    ))

;; infer-type : Expr TypeEnv -> Type
; toma una expresión y un ambiente de tipos y devuelve su Type
(define (infer-type expr tenv)
  (match expr
    [(num n) (numT)]
    [(tt) (boolT)]
    [(ff) (boolT)]
    [(binop op l r) (if
                     (and (equal? (infer-type l tenv) (numT))
                          (equal? (infer-type r tenv) (numT)))
                     (if (equal? op '<= ) (boolT) (numT)) (error (format "infer-type: invalid operand type for ~a" op ) ))]
    [(ifc c th el) (if (equal? (infer-type c tenv) (boolT))
                       (if (equal? (infer-type th tenv) (infer-type el tenv))
                           (infer-type th tenv) (error "infer-type: if branches type mismatch"))
                       (error "infer-type: if condition must be a boolean"))]

    [(id x) (tenv-lookup x tenv)]
    [(fun binder binderType body) (arrowT binderType (infer-type body (extend-tenv binder binderType tenv)))]
    [(app callee arg) (match (infer-type callee tenv)
                        [(arrowT t1 t2) (if (equal? t1 (infer-type arg tenv)) t2 (error "infer-type: function argument type mismatch"))]
                        [_ (error "infer-type: function application to a non function")]
                        )]
    ))

#| END P1 |#

#| BEGIN P2 PREAMBLE |#

;; ambiente de sustitución diferida
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; interface ADT (abstract data type) del ambiente
(define empty-env (mtEnv))

;; "Simplemente" asigna un nuevo identificador para aEnv
;(define extend-env aEnv)
;;
;; es lo mismo que definir extend-env así:
;; (concepto técnico 'eta expansion')
(define (extend-env id val env) (aEnv id val env))

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x) val (env-lookup x rest))]))

;; num2num-op : (Number Number -> Number) -> Val Val -> Val
(define (num2num-op op)
  (λ (l r)
    (match (cons l r)
      [(cons (num n) (num m)) (num (op n m))]
      [_ (error 'num-op "invalid operands")])))


(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))

#| END P2 PREAMBLE |#

#| BEGIN P2 |#
;; final? : Expr -> Boolean
; Dada  una  expresión  determina  si  es  un valor o  no.
(define (final? e)
  (match e
    [(num n) #t]
    [(binop op l r) #f]
    [(id x ) #f]
    [(fun binder binderType body) #t]
    [(app callee arg) #f]
    ))



#|
;; Kont ::= <numT>    (revisar)
         |(<sym> <expr> <Env> <Kont>)
         |(<sym> <expr> <Env> <Kont>)
         | (expr> <Env> <Kont>)
         | (expr> <Env> <Kont>)

|#

(deftype Kont
  (mt-k)
  (binop-l-k op r env kont)
  (binop-r-k op l env kont)
  (arg-k arg env kont )
  (fun-k fun env kont)
  )


(define empty-kont (mt-k))

;; State ::= (<Expr>, <Env>, <Kont>)
(deftype State
  (st expr env kont)
  )

;; inject : Expr -> State
; toma una expresión y devuelve un estado inicial vacío
(define (inject expr)
  (st expr empty-env empty-kont)
  )

;; step: State-> State
; toma un estado y devuelve el siguiente estado
(define (step c)
  (define operator-table
    (hash '+ num+ '- num- '* num*))
  (match-define (st c-exp c-env c-kont) c )
  (match c-exp
    [(binop op l r) (st l c-env (binop-r-k op r c-env c-kont))]
    [(id x) (st (car (env-lookup x c-env)) (cdr (env-lookup x c-env)) c-kont)]
    [(app callee  arg ) (st callee empty-env (arg-k arg  empty-env  empty-kont))]
    [_ (match c-kont
         [(binop-r-k op this-l this-env this-kont) (st this-l this-env (binop-l-k op c-exp c-env this-kont))]
         [(binop-l-k op this-r env this-kont) (st ((hash-ref operator-table op) this-r c-exp) c-env this-kont)]
         [(arg-k this-arg this-env this-kont) (st this-arg this-env (fun-k c-exp this-env this-kont))]
         [(fun-k this-fun this-env this-kont)
          (match-define (fun binder bindertype body) this-fun)
          (st body
              (extend-env binder (cons c-exp c-env) c-env)  empty-kont)]
         )]
    )
  )

;; eval : Expr -> Expr
(define (eval expr)
  (define (eval-until-final state)
    (def (st expr _ kont) state)
    (if (and (final? expr) (mt-k? kont))
        expr
        (eval-until-final (step state))))
  (eval-until-final (inject expr)))

;; run : ...
(define (run s-expr)
  (define (loop state  condition function type)
    (displayln state) 
    (if condition
        (cons state type)  
        (loop (function state) condition function)))

  (define expr (parse s-expr))
  (define final-expr (eval expr))
  (define type (infer-type expr empty-tenv))
  (cons final-expr type)
  )
  
