#lang sicp

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (log2 x)
  (/ (log x) (log 2)))

(define (log3 x)
  (/ (log x) (log 3)))

(define (car z)
  (if (= 0 (remainder z 3))
      (car (/ z 3))
      (log2 z)))

(define (cdr z)
  (if (= 0 (remainder z 2))
      (cdr (/ z 2))
      (log3 z)))

(define test (cons 344 230))
(car test)
(cdr test)