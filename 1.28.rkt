#lang sicp
(#%require (only racket random))
(define (sicp-random n)
  (if (exact? n)
    (random n)
    (* n (random))))

(define (square x)
  (* x x))

(define (square-with-check x m)
  (if (and (= 1 (remainder (square x) m))
            (not (= x 1))
            (not (= x (- m 1))))
      0
      (square x)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square-with-check (expmod base (/ exp 2) m) m)
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                       m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (sicp-random (- n 1)))))

 (define (fast-prime? n times) 
   (cond ((= times 0) true) 
         ((miller-rabin-test n) (fast-prime? n (- times 1))) 
         (else false)))

 (define (prime? n)
    (fast-prime? n 100))

(prime? 3)
; #t
(prime? 4)
; #f
(prime? 5)
; #t
(prime? 100)
; #f
(prime? 561)
; #f
(prime? 1105)
; #f
(prime? 37)
; #t
(prime? 39)
; #f
(prime? 15485867)
; #t