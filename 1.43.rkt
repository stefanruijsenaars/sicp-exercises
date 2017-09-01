#lang sicp

(define (compose f g) (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))