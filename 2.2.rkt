#lang sicp
(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let (
        (start-x (x-point (start-segment s)))
        (end-x   (x-point (end-segment s)))
        (start-y (y-point (start-segment s)))
        (end-y   (y-point (end-segment s)))
       )
    (make-point (average start-x end-x) (average start-y end-y))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment (make-segment (make-point 2 2) (make-point 4 6))))