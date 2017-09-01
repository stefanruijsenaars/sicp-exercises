#lang sicp

(define (reverse list)
  (if (null? list)
      '()
      (append (reverse (cdr list)) (cons (car list) '()))))

(reverse (list 23 27 149 34))
