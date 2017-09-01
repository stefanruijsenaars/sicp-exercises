#lang sicp
(define (even? n)
  (= (remainder n 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (* b n)
  (mult-iter b n 0))

; multiplies b and n. a + b*n remains constant (equal to b*n)
(define (mult-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (mult-iter (double b) (halve n) a))
        (else (mult-iter b (- n 1) (+ a b)))))