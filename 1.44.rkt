#lang sicp

(define (compose f g) (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define dx 0.000001)

(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(smooth square)
((smooth square) 5)
(n-fold-smooth square 2)
((n-fold-smooth square 2) 5)