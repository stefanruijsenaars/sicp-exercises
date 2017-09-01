#lang sicp
(#%require (only racket random))
(define (sicp-random n)
  (if (exact? n)
    (random n)
    (* n (random))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                       m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (sicp-random (- n 1)))))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (test1 start primesfound)
  (if (= primesfound 3)
      "  complete "
      (if (= 0 (remainder start 2))
          (test1 (+ start 1) primesfound)
          (cond ((fast-prime? start 10) (timed-prime-test start)
                            (test1 (+ start 1) (+ 1 primesfound)))
                (else  (test1 (+ start 1) primesfound))))))
                    

(test1 1000 0)
(test1 10000 0)
(test1 100000 0)
(test1 1000000 0)
(test1 100000000 0)
(test1 100000000 0)
(test1 1000000000 0)
      