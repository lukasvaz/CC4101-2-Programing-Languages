#lang play
(require "T3.rkt")
(print-only-errors #t)


; Parte 1 a)
(test (parse-type 'Number) (numT))
(test (parse-type '(-> Number Number)) (arrowT (numT) (numT)))
(test (parse-type '(->(-> Number Number)  Number) ) (arrowT (arrowT (numT) (numT)) (numT)))

; Parte 1 b)
(test (infer-type (num 1) empty-tenv) (numT))
(test (infer-type (fun 'x  (numT) (id 'x)) empty-tenv) (arrowT (numT) (numT)))
(test (infer-type (app (fun 'x (numT) (id 'x)) (num 2)) empty-tenv) (numT) )
(test (infer-type (binop '+ (num 3) (num 4)) empty-tenv) (numT) )
(test (infer-type (binop '- (num 3) (num 4)) empty-tenv) (numT) )
(test (infer-type (binop '* (num 3) (num 4)) empty-tenv) (numT) )
(test (infer-type (id 'x) (extend-tenv 'x (numT) empty-tenv)) (numT) )

(test/exn (infer-type (binop '+ (num 1) (fun 'x (numT) (id 'x))) empty-tenv) "infer-type: invalid operand type for +")
(test/exn (infer-type (binop '+ (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x))) empty-tenv) "infer-type: invalid operand type for +")
(test/exn (infer-type (app (num 1) (num 2) ) empty-tenv) "infer-type: function application to a non function")
(test/exn (infer-type (app (fun 'x (numT) (id 'x)) (fun 'x (numT) (id 'x)) ) empty-tenv) "infer-type: function argument type mismatch")


;Parte 1 c)
(test (parse-type 'Boolean) (boolT) )
(test (parse-type '(-> Boolean Boolean)) (arrowT (boolT) (boolT)))
(test (parse-type '(-> Boolean Number)) (arrowT (boolT) (numT)))
(test (parse-type '(-> Number Boolean)) (arrowT (numT) (boolT)))
(test (parse-type '(->(-> Boolean Boolean) Boolean)) (arrowT (arrowT (boolT) (boolT)) (boolT) ))

(test (parse 'true) (tt))
(test (parse 'false) (ff))
(test (parse '(<= 2 3)) (binop '<= (num 2) (num 3)))  
(test  (parse '( if (<= 5 6) true false )) (ifc (binop '<= (num 5) (num 6)) (tt)(ff)) )


(test ( infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (num 3)) empty-tenv) (numT) )
(test ( infer-type ( ifc (binop '<= (num 10) (num 20)) (tt) (ff)) empty-tenv) (boolT) )

(test/exn (infer-type ( ifc (num 5) (num 2) (num 3)) empty-tenv)
          "infer-type: if condition must be a boolean")

(test/exn (infer-type ( ifc (fun 'x (numT) (id 'x)) (num 2) (num 3)) empty-tenv)
          "infer-type: if condition must be a boolean")

(test/exn ( infer-type ( ifc (binop '<= (num 5) (num 6)) (num 2) (tt)) empty-tenv)
          "infer-type: if branches type mismatch" )

(test/exn ( infer-type ( ifc (binop '<= (num 5) (num 6)) (fun 'x (numT) (id 'x)) (num 2) ) empty-tenv)
          "infer-type: if branches type mismatch" )

; Parte 2 a)
(test (final? (num 1)) #t)
(test ( final? (fun 'x (numT) (id 'x))) #t)
(test ( final? (binop '+ (num 1) (num 2))) #f)
(test ( final? (binop '- (num 1) (num 2))) #f)
(test ( final? (binop '* (num 1) (num 2))) #f)
(test ( final? (app (fun 'x (numT) (id 'x)) (num 10)) ) #f)
(test ( final?  (id 'x))  #f)

; Parte 2e)
(test (step (st (binop '+ (num 1) (num 2)) (mtEnv) (mt-k)))
      (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k))) )

(test (step (st (num 1) (mtEnv) (binop-r-k '+ (num 2) (mtEnv) (mt-k))))
      (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k))) )

(test (step (st (num 2) (mtEnv) (binop-l-k '+ (num 1) (mtEnv) (mt-k))))
      (st (num 3) (mtEnv) (mt-k)))

(test (step (st (app (fun 'x (numT) (id 'x)) (num 2))(mtEnv) (mt-k)))
      (st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2)(mtEnv)(mt-k))))

(test (step (st (fun 'x (numT) (id 'x)) (mtEnv) (arg-k (num 2)(mtEnv)(mt-k))))
      (st (num 2)(mtEnv)(fun-k (fun 'x (numT) (id 'x)) (mtEnv) (mt-k))))

(test (step (st (num 2)(mtEnv)(fun-k (fun 'x (numT) (id 'x)) (mtEnv) (mt-k))))
      (st (id 'x)(extend-env 'x (cons (num 2) (mtEnv)) (mtEnv))(mt-k)) )

(test (step (st (id 'x) (extend-env 'x (cons (num 2) (mtEnv)) (mtEnv))(mt-k)))
      (st (num 2) (mtEnv) (mt-k)) )


; Parte2 f
(test (run '(+ 1 2)) (cons (num 3) (numT)))
(test (run '(- 1 2)) (cons (num -1) (numT)))
(test (run '(* 1 2)) (cons (num 2) (numT)))
(test (run '(+(* 1 2) 3)) (cons (num 5) (numT)))
(test (run '((fun (x : Number) x) 2))  (cons (num 2) (numT) ) )
(test (run '((fun (x : Number) x) (+ 2 3) ))  (cons (num 5) (numT) ) )
(test (run '((fun (x : Number) (+ x 10)) 3 ))  (cons (num 13) (numT) ) )

(test (run '(fun (x : Number) (+ x 10)))  (cons (fun 'x (numT) (binop '+ (id 'x) (num 10)))  (arrowT (numT) (numT)) ) )
(test (run '(fun (x : Number) (fun (y : Number) (* x y ))))  (cons (fun 'x (numT) (fun 'y (numT) (binop '* (id 'x) (id 'y) )))  (arrowT (numT) (arrowT (numT) (numT))) ) )

