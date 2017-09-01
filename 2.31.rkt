#lang sicp

(define (square x)
  (* x x))


(define (tree-map f t)
  (map (lambda (st)
         (cond ((pair? st)
                (tree-map f st))
               ((null? st) '())
               (else (f st))))
       t))

(newline)
(display (tree-map square
  (list '() 1
        (list 2 (list 3 4) 5)
        (list 6 7))))