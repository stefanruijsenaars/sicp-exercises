#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        ; union of the set of all subsets excluding the car
        (append rest
                ; and the set of all subsets excluding the car, with the car consed on to it.
                (map (lambda (x) (cons (car s) x)) rest)))))