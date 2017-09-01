#lang sicp

(define (product-linear-recursion term a next b) 
   (if (> a b) 
       0 
       (* (term a) 
          (product-linear-recursion term (next a) next b))))

(define (product term a next b) 
   (define (iter a result)
     (if (> a b)
         result
         (iter (next a) (* result (term a)))))
   (iter a 1))

(define (identity x) x)

(define (factorial x)
  (product identity 1 inc x))

(define (get-approx-pi-term x)
  (define a (if (odd? x)
                (+ x 1)
                (+ x 2)))
  (define b (if (odd? x)
                (+ x 2)
                (+ x 1)))
  (* (/ a b) 1.0))

(* 4 (product get-approx-pi-term 1 inc 1000))