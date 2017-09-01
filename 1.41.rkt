#lang sicp

(define tolerance 0.000001)

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
       (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))
         
(define  (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define dx 0.000001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)))
    dx))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))