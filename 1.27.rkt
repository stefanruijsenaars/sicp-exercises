#lang sicp

(define (square x)
  (* x x))

(define (iter acc n)
  (cond ((= acc n) true)
        ((= (expmod acc n n) acc) (iter (+ acc 1) n))
        (else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                       m))))

(define (carmichael-test num)
  (iter 1 num))

; is a prime
(carmichael-test 3)
; is not a prime
(carmichael-test 4)
; is a prime
(carmichael-test 5)
; carmichael numbers - are not primes
(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)