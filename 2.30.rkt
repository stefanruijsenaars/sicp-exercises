#lang sicp

(define (square x)
  (* x x))

(define (square-tree t)
  (cond ((null? t) '())
        ((pair? t) (cons
                    (square-tree (car t))
                    (square-tree (cdr t))))
        (else (square t))))

(display (square-tree
  (list '() 1
        (list 2 (list 3 4) 5)
        (list 6 7))))

(define (square-tree-2 t)
  (map (lambda (st)
         (cond ((pair? st)
                (square-tree-2 st))
               ((null? st) '())
               (else (square st))))
       t))

(newline)
(display (square-tree-2
  (list '() 1
        (list 2 (list 3 4) 5)
        (list 6 7))))