#lang sicp

(define (for-each f list)
  (if (null? list)
      (newline)
      ((lambda () 
         (f (car list))
         (for-each f (cdr list))))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))