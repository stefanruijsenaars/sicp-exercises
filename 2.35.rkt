#lang sicp

(define (accumulate f acc list)
  (if (null? list)
      acc
      (f (car list)
         (accumulate f acc (cdr list)))))

(define x (cons (list 1 (list) 2) (list 3 4)))
(define t (list x x))

(define (count-leaves-old t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves-old (car t))
                 (count-leaves-old (cdr t))))))

(define (count-leaves t)
;  (accumulate ??? ??? (map ?? ??))
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves node)
                             (if (null? node)
                                 0
                                 1)))
                       t)))

(count-leaves t)