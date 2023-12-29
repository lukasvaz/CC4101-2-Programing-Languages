#lang play
(require "T2.rkt")


(print-only-errors #t)

; p1 a)
(test (parse 1) (num 1))
(test (parse 'true) (tt))
(test (parse 'false) (ff))
(test (parse'(+ 3 4)) (add (num 3) (num 4)))
(test (parse'(- 3 4)) (sub (num 3) (num 4)))
(test (parse'(* 3 4)) (mul (num 3) (num 4)))
(test (parse'(<= 3 4)) (leq (num 3) (num 4)))
(test (parse'(if (<= 3 5)  2 4)) (ifc  (leq (num 3) (num 5)) (num 2)(num 4)))
(test (parse '(+ 3 (- 2 5)))(add (num 3) (sub  (num 2) (num 5))))

; p1 b )
; definicion de  ids
(test (parse 'x) (id 'x))
; definicion de funciones
(test (parse '(fun (x) x)) (fun (list 'x) (id 'x) ))
(test (parse '(fun (x y) (+ x y))) (fun (list 'x 'y) (add (id 'x) (id 'y))))
(test (parse '(fun (x y) (if (<= x y) true  false ))) (fun (list 'x 'y) (ifc  (leq (id 'x) (id 'y)) (tt) (ff))))
; aplicacion de fucniones
(test (parse '(my-function 2 3 4) ) (app (id 'my-function) (list (num 2)(num 3)(num 4))))
(test (parse '(my-function 2 3 4 5) ) (app (id 'my-function) (list (num 2)(num 3)(num 4) (num 5))))
; funcion anonima
(test (parse '((fun (x) x) 3) ) (app(fun (list 'x) (id 'x) ) (list (num 3))))


; p1 d)
(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))
(define num<= (num2bool-op <=))

(test (num+ (numV 3)(numV 4)) (numV 7))
(test (num- (numV 7)(numV 4)) (numV 3))
(test (num* (numV 10)(numV 4)) (numV 40))
(test (num<= (numV 3)(numV 4)) (boolV #t))
(test (num<= (numV 4)(numV 3)) (boolV #f))

(test/exn (num+ (boolV #t)(numV 3)) "num-op:invalid operands")
(test/exn (num<= (boolV #t)(numV 3)) "num-op:invalid operands")

; p1e
; eval  abstract sintax
(test (eval (num 3)empty-env)(numV 3))
(test (eval (tt)empty-env)(boolV #t))
(test (eval (ff)empty-env)(boolV #f))
(test (eval (add (num 3) (num 4)) empty-env)(numV 7))
(test (eval (sub (num 5) (num 4)) empty-env)(numV 1))
(test (eval (mul (num 5) (num 4)) empty-env)(numV 20))
(test (eval (leq (num 5) (num 4)) empty-env)(boolV #f))
(test (eval (leq (num 4) (num 5)) empty-env)(boolV #t))
(test (eval (ifc (tt) (num 0) (num 1)) empty-env ) (numV 0))
(test (eval (ifc (ff) (num 0) (num 1)) empty-env ) (numV 1))
(test (eval (fun (list 'x) (id 'x)) empty-env ) (clousureV '(x) (id 'x) empty-env))
(test (eval (app (fun '(x) (add (id 'x) (num 10))) (list (num 2))) empty-env ) (numV 12 ))
(test (eval (app (fun '(x y) (add (id 'x) (id 'y))) (list (num 2) (num 20))) empty-env ) (numV 22))

; eval  from  concrete sintax (parser -> interpret)
(test (eval(parse 'x) (extend-env 'x (numV 4)  empty-env)) (numV 4))
(test (eval(parse '(fun (x) x)) empty-env ) (clousureV '(x) (id 'x) empty-env))
(test (eval(parse '(fun (x y) (+ x y))) empty-env ) (clousureV '(x y) (add (id 'x) (id 'y)) empty-env))
(test (eval(parse '(fun (x y) (if (<= x y) true  false ))) empty-env )
      (clousureV '(x y) (ifc (leq (id 'x)  (id 'y)) (tt) (ff)) empty-env))
(test (eval (parse '((fun (x) x) 3)) empty-env) (numV 3))
(test (eval (parse '(my-function 2 3 4))
            (aEnv 'my-function (clousureV '(x y z) (add (id 'x) (add (id 'y) (id 'z))) empty-env) empty-env) )
      (numV 9))


; p1f

(test (parse '(tuple 1 2 3)) (tupl (list (num 1)(num 2)(num 3))) )
(test (parse '(tuple 1 2 3 4 )) (tupl (list (num 1)(num 2)(num 3) (num 4))) )
(test (parse '(tuple)) (tupl (list)) )
(test (parse '(proj (tuple 10 20 30 ) 1)) (proj (tupl (list  (num 10) (num 20) (num 30)) ) (num 1)))
(test (parse '(proj (tuple 10 20 30 ) 0)) (proj (tupl (list  (num 10) (num 20) (num 30)) ) (num 0)))

; p1 g
(test (eval (tupl (list (num 10) (num 20 ) (num 30))) empty-env) (tupleV (list (numV 10) (numV 20 ) (numV 30))  ) )
(test (eval (tupl (list  (add (num 10) (num 5)) (num 20 )  ) ) empty-env )(tupleV (list (numV 15) (numV 20 ))) )
(test (eval (proj (tupl (list  (sub (num 10) (num 5)) (num 20))) (num 1))  empty-env ) (numV 20))
(test (eval (proj (tupl (list  (sub (num 10) (num 5)) (num 20))) (num 0))  empty-env ) (numV 5))


; p2 a
; swap
(define my-program-swap (parse '((swap (fun (a b) (- a b) ))  10 5)))
(test (eval my-program-swap  (extend-env 'swap swap* empty-env )) (numV -5))
(define my-program-swap-2 (parse '((swap (fun (a b) (<= a b) ))  10 5)))
(test (eval my-program-swap-2  (extend-env 'swap swap* empty-env )) (boolV #t))

; curry
(define my-program-curry (parse '( ((curry (fun (a b) (+ a b))) 10) 5)))
(test (eval my-program-curry  (extend-env 'curry curry* empty-env )) (numV 15))

(define my-program-curry-2 (parse '( ((curry (fun (a b) (- a b))) 10) 5)))
(test (eval my-program-curry-2  (extend-env 'curry curry* empty-env )) (numV 5))

; uncurry
(define my-program-uncurry (parse '((uncurry (curry (fun (a b) (+ a b)))) 10 5 )))
(test (eval my-program-uncurry
            (extend-env 'curry curry* (extend-env 'uncurry uncurry* empty-env ))) (numV 15))


(define my-program-uncurry-2 (parse '((uncurry (curry (fun (a b) (- a b)))) 10 5 )))
(test (eval my-program-uncurry-2
            (extend-env 'curry curry* (extend-env 'uncurry uncurry* empty-env ))) (numV 5))

; partial
(define my-program-partial (parse '((partial (fun (a b) (+ a b)) 10 ) 5) ))
(test (eval my-program-partial
            (extend-env 'partial partial* empty-env )) (numV 15))

(define my-program-partial-2  (parse '((partial (fun (a b) (- a b)) 10 ) 5) ))
(test (eval my-program-partial-2
            (extend-env 'partial partial* empty-env )) (numV 5))


; run
(define  globals
  (list (cons 'swap swap*) (cons 'curry curry*) (cons 'uncurry uncurry*) (cons 'partial partial*)))
(test (run '((swap (fun (x y) (<= x y))) 1 2) globals) (boolV #f))

(test (run
'((swap (fun (a b) (- a b) ))  10 5)
       globals )
      (numV -5))
  
(test (run
       ' ( ((curry (fun (a b) (+ a b))) 10) 5)
       globals )
      (numV 15))
      

(test (run
       '(( partial (fun (x y) (<= x y)) 1) 2)
       globals )
      (boolV #t))

(test (run
       '((uncurry (curry (fun (a b) (- a b)))) 10 5 )
       globals )
      (numV 5))
