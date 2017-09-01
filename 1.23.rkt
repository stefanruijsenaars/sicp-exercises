#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next x)
  (if (= x 2) 3
      (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (test1 start primesfound)
  (if (= primesfound 3)
      "  complete "
      (if (= 0 (remainder start 2))
          (test1 (+ start 1) primesfound)
          (cond ((prime? start) (timed-prime-test start)
                            (test1 (+ start 1) (+ 1 primesfound)))
                (else  (test1 (+ start 1) primesfound))))))
                    

(test1 1000 0)
(test1 10000 0)
(test1 100000 0)
(test1 1000000 0)
(test1 10000000 0)
(test1 100000000 0)
(test1 1000000000 0)
      