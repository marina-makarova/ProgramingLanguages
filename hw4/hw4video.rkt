#lang racket
(provide (all-defined-out))
(define s "hello")
(define (my-map f xs)
  (if(null? xs)
     null
     (cons (f (car xs)) (my-map f (cdr xs)))))
(define (silly-double x)
  (let ([x (+ x 3)]
        [y (+ x 2)])
        (+ x y -5)))

(define (fact x) 
  (if (= x 0)
      1 
      (* x (fact (- x 1)))))

(define (my-if-bad e1 e2 e3)
  (if e1 (e2) (e3)))

(define (test-bad-if x)
  (my-if-bad (= x 0) 
             1 
             (* x (test-bad-if (- x 1)))))