#lang sicp

(define (accumulate f acc list)
  (if (null? list)
      acc
      (f (car list)
         (accumulate f acc (cdr list)))))

(define (accumulate-n f acc seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate f acc (map car seqs))
            (accumulate-n f acc (map cdr seqs)))))


(accumulate-n + 0 (list (list 1 2 3) (list 3 4 5) (list 7 8 9)))