#lang sicp

(define (reverse list)
  (if (null? list)
      '()
      (append (reverse (cdr list)) (cons (car list) '()))))

(reverse (list 23 27 149 34))
(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse list)
  (cond ((null? list) '())
        ((pair? (car list)) (append (deep-reverse (cdr list)) (cons (deep-reverse (car list)) '())))
        (else (append (deep-reverse (cdr list)) (cons (car list) '())))))