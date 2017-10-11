#lang sicp

;a
((lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (f k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (f f (- k 1))
                     (f f (- k 2)))))))) 6)

;b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(f 3)
(f 4)