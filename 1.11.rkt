#lang sicp

(define (f n)
  (if (< n 3)
   n
   (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f2 n)
  (f-iter 3 n 4 2 1 0))

(define (f-iter current-n n current-result fnminus1 fnminus2 fnminus3)
  (if (< n 3)
   n
   (if (= n current-n)
       current-result
       (f-iter (+ current-n 1) n (+ current-result (* 2 fnminus1) (* 3 fnminus2)) current-result fnminus1 fnminus2))))