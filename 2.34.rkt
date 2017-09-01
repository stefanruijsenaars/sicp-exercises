#lang sicp

(define (accumulate f acc list)
  (if (null? list)
      acc
      (f (car list)
         (accumulate f acc (cdr list)))))

(define (horner-eval x coefficient-sequence)
  (+ (car coefficient-sequence) (accumulate (lambda (this-coeff acc) (* (+ this-coeff acc) x))
              0
              (cdr coefficient-sequence))))

(horner-eval 2 (list 1 3 0 5 0 1))