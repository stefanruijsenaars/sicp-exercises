#lang sicp

(define (square x)
  (* x x))

(define (square-list-map items)
  (map square items))

(square-list-map (list 1 2 3 4))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (append answer (list (square (car things)))))))
  (iter items nil))

(square-list (list 1 2 3 4))