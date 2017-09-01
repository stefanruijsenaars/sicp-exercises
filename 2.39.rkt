#lang sicp

(define (fold-right f acc list)
  (if (null? list)
      acc
      (f (car list)
         (fold-right f acc (cdr list)))))
      
   
(define (fold-left f acc list)
  (define (iter result rest)
    (if (null? rest)
         result
         (iter (f result (car rest))
               (cdr rest))))
  (iter acc list))

(define (reverse-left list)
  (fold-left (lambda (acc cur) (cons cur acc)) nil list))

(define (reverse-right items)
  (fold-right (lambda (first acc)
                (append acc (list first)))
              nil
              items))

(reverse-left (list 1 2 3 4))
(reverse-right (list 1 2 3 4))