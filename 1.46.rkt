#lang sicp

(define (square x)
  (* x x))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-legacy x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; takes 2 functions
(define (iterative-improve close-enough? improve)
  (lambda (guess)
    (let ((improved (improve guess)))
      (if (close-enough? guess improved)
         improved
         ((iterative-improve close-enough? improve) improved)))))

(define (sqrt x)
  ((iterative-improve
   (lambda (v1 v2) (< (abs (- v1 v2)) 0.001))
   (lambda (guess) (average guess (/ x guess))))
   1.0))


(sqrt 4)
(sqrt-legacy 4)

(define tolerance 0.00001) 
  
(define (fixed-point-legacy f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
   (define (try guess)
     (let ((next (f guess)))
       (if (close-enough? guess next)
           next
           (try next))))
   (try first-guess))


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
  ((iterative-improve close-enough? f) first-guess))



(fixed-point-legacy cos 1.0)
(fixed-point cos 1.0)