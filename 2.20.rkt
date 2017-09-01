#lang sicp

(define (same-parity parity . items)
  (define (recurse items)
   (if (null? items)
       '()
       (if (or (and (even? (car items)) (even? parity)) (and (odd? (car items)) (odd? parity)))
           (cons (car items) (recurse (cdr items)))
           (recurse (cdr items)))))
    
   (cons parity (recurse items)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

