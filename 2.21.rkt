#lang sicp

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))

(define (square-list-map items)
  (map square items))

(square-list-map (list 1 2 3 4))