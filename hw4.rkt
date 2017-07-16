#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; not correct if n is negative multiple of the length of xs
;(define (list-n-mod-rec xs n)
;  (let ([nn (remainder n (length xs))])
;    (cond
;      [(< nn 0) (error "list-nth-mod: negative number")]
;      [(null? xs) (error "list-nth-mod: empty list")]
;      [(= nn 0) (car xs)]
;      [#t (list-n-mod-rec (cdr xs) (- nn 1))])))

(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]))
  

(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

(define funny-number-stream (lambda ()
  (letrec ([f (lambda (ans)
           (cons (if (= 0 (remainder ans 5))
                     (- ans)
                     ans)
                 (lambda () (f (+ (abs ans) 1)))))])
    (f 1))))

(define dan-then-dog
  (lambda () (letrec ([f (lambda (img)
                           (cons img (lambda ()
                                       (f (if (equal? img "dog.jpg")
                                              "dan.jpg"
                                              "dog.jpg")))))])
               (f "dan.jpg"))))

; sample solution
(define dan-then-dog-sample
  (letrec ([dan-st (lambda () (cons "dan.jpg" dog-st))]
           [dog-st (lambda () (cons "dog.jpg" dan-st))])
    dan-st))

(define (stream-add-zero s)
  (lambda ()
    (define pr (s))
    (cons (cons 0 (car pr)) (stream-add-zero (cdr pr)))))

(define (cycle-lists xs ys)
  (define (proc n)
    (cons (cons (list-nth-mod xs n)
                (list-nth-mod ys n))
          (lambda () (proc (+ n 1)))))
  (lambda () (proc 0)))

; the if-flow can be written more simply using and
(define (vector-assoc v vec)
  (letrec ([search (lambda (n)
                     (cond
                       [(= n (vector-length vec)) #f]
                       [(not (pair? (vector-ref vec n))) (search (+ n 1))] 
                       [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
                       [#t (search (+ n 1))]))])
    (search 0)))

; the binding for f is redundant, can just return the lambda
(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [cur 0]
         [f (lambda (v)
              (let ([target (vector-assoc v cache)])
                (if target
                    target
                    (let ([newval (assoc v xs)])
                      (if newval
                          (begin
                            (println "new cache added")
                            (vector-set! cache cur newval)
                            (set! cur (+ cur 1))
                            (set! cur (if (= n cur) 0 cur))
                            newval)
                          #f)))))])
    f))

(define my-assoc (cached-assoc (list (cons 1 2) (cons 3 4) (cons 9 0)) 2))

;(begin
;  (define a (my-assoc 1))
;  (println a)
;  (define b (my-assoc 2))
;  (println b)
;  (define c (my-assoc 1))
;  (define d (my-assoc 12))
;  (define e (my-assoc 3))
;  (define f (my-assoc 2))
;  (define g (my-assoc 1)))

; wrong
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([r1 e1]
              [r2 e2]
              [loop (lambda (it)
                      (if (< r1 it)
                          #t
                          (loop e2)))])
       (loop r2))]))

; sample
(define-syntax while-less-sample
  (syntax-rules (do)
    ((while-less x do y)
      (let ([z x])
        (letrec ([loop (lambda ()
			                  (let ([w y])
		 	                    (if (or (not (number? w)) (>= w z))
			                        #t
			                        (loop))))])
          (loop))))))