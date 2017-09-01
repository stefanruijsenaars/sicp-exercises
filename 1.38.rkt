#lang sicp

(define (cont-frac n d k)
  (define (do-it i)
    (/ (n i)
       (if (= i k)
         (d i)
         (+ (d i) (do-it (+ i 1))))))
  (do-it 1))

;should be: 2.718281828459045235360
(+ 2 (cont-frac (lambda (i) 1.0) (lambda (i) (if (= 0 (remainder (+ i 1) 3)) (* (/ (+ i 1) 3) 2) 1)) 10000))