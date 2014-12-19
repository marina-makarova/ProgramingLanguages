#lang racket
(provide (all-defined-out))

(define (sequence low high stride)
  (if (< high low) 
  null
  (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda(x) 
         (string-append x suffix))
        xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))
(define ones (lambda() (cons 1 ones)))


(define nats 
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define funny-number-stream
    (letrec ([f (lambda (x) 
                  (let ([x1 (if (= 0 (remainder x 5))
                                (- x)
                                x)])
                  (cons x1 (lambda () (f (+ x 1))))))])
(lambda () (f 1))))


(define dan-then-dog
  (letrec ([dan (lambda (x) (cons (if x "dan.jpg" "dog.jpg")
                                  (lambda () (dan (not x)))))])
    (lambda () (dan #t))))

(define (stream-add-zero s)
  (letrec ([zerof (lambda(s) (cons (cons 0 (car (s)))
                                   (lambda () (zerof (cdr (s))))))])
    (lambda() (zerof s))))
  
(define (cycle-lists xs ys)
  (letrec ([zip (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                   (lambda () (zip (+ n 1)))))])
    (lambda () (zip 0))))

(define (stream-for-n-steps stream n)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                (if (= n ans)
                    null
                    (cons (car pr) (f (cdr pr) (+ ans 1))))))])
                     (f stream 0)))
                 