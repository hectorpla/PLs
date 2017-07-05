;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rktlist)
  (if (empty? rktlist)
      (aunit)
      (let ([rkt-elem (car rktlist)]
            [rest (cdr rktlist)])
        (apair rkt-elem (racketlist->mupllist rest)))))

(define (mupllist->racketlist mupllist)
  (if (aunit? mupllist)
      null
      (let ([mupl-elem (apair-e1 mupllist)]
            [rest (apair-e2 mupllist)])
        (cons mupl-elem (mupllist->racketlist rest)))))
                  
;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        ; values
        [(int? e) e]
        [(closure? e) e]
        [(aunit? e) e]
        ; expressions
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (ifgreater-e3 e)
                   (ifgreater-e4 e))
               (error "MUPL ifgreater applied to non-number")))]
        [(call? e)
         (if (closure? (call-funexp e))
             (let* ([fun (closure-fun (call-funexp e))]
                    [fun-name (fun-nameopt fun)]
                    [fun-arg (fun-formal fun)]
                    [fun-body (fun-body fun)]
                    [val (eval-under-env (call-actual e) env)]
                    [new-env (cons (cons fun-arg val) env)])
               (let ([final-env (if fun-name
                                    (cons (cons fun-name (call-funexp e) new-env))
                                    new-env)])
                     (eval-under-env fun-body final-env)))
             (error "MUPL call applied to non-closure"))]
        [(mlet? e)
         (let* ([str (mlet-var e)]
                [val (eval-under-env (mlet-e e) env)]
                [new-env (cons (cons str val) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
                  (apair v1 v2))]
        [(fst? e)
         (if (apair? (fst-e e))
             (let ([pr (eval-under-env (fst-e e) env)])
               (apair-e1 pr))
             (error "MUPL fst applied to non-pair"))]
        [(snd? e)
         (if (apair? (snd-e e))
             (let ([pr (eval-under-env (snd-e e) env)])
               (apair-e2 pr))
             (error "MUPL snd applied to non-pair"))]
        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([elem (car lstlst)])
        (mlet (car elem) (cdr elem) (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y")
                    e4
                    (ifgreater (var "_y") (var "_x")
                               e4
                               e3))))

;; Problem 4

(define mupl-map "CHANGE")

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))