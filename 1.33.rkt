#lang sicp

(define (square x) (* x x))

(define (smallest-div n)
  (define (divides? a b)
    (= 0 (remainder b a)))
  (define (find-div n test)
    (cond ((> (square test) n) n) ((divides? test n) test)
          (else (find-div n (+ test 1)))))
  (find-div n 2))

; ensure prime of 1 is false
(define (prime? n)
  (if (= n 1) false (= n (smallest-div n))))

(define (filtered-accumulate combiner null-value term a next b filter) 
   (define (iter a result)
     (if (> a b)
         result
         (if (filter a)
            (iter (next a) (combiner result (term a)))
            (iter (next a) result))))
   (iter a null-value))


(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (gcd m n)
  (cond ((< m n) (gcd n m))
        ((= n 0) m)
        (else (gcd n (remainder m n))))) 

(define (relative-prime? m n)
  (= (gcd m n) 1))

(define (identity x) x)

(define (product-lesser-relative-prime n)
  (define (filter x)
    (relative-prime? x n))
  (filtered-accumulate * 1 identity 1 inc n filter))