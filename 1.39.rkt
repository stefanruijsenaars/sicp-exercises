#lang sicp

(define (cont-frac n d k)
  (define (do-it i)
    (/ (n i)
       (if (= i k)
         (d i)
         (+ (d i) (do-it (+ i 1))))))
  (do-it 1))

(define (tan-cf x k)
  (* 1.0 (cont-frac (lambda (i) (if (= i 1) x (- (* x x)))) (lambda (i) (- (* i 2) 1)) k)))