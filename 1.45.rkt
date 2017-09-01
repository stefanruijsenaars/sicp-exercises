#lang sicp

(define (compose f g) (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (iterate start improve good-enough?)  
   (if (good-enough? start)  
       start  
       (iterate (improve start) improve good-enough?)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
       (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))

(define (root2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
  1.0))

(define (root3 x)
  (fixed-point (average-damp (lambda (y) (/ x (expt y 2))))
  1.0))

; doesn't terminate without avg damp 2
(define (root4 x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x  (expt y 3))))
  1.0))

(define (root5 x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x  (expt y 4))))
  1.0))

(define (root6 x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x  (expt y 5))))
  1.0))

(define (root7 x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x  (expt y 6))))
  1.0))

; doesnt terminate with 2 repeats
(define (root8 x)
  (fixed-point ((repeated average-damp 3)
                (lambda (y) (/ x  (expt y 7))))
  1.0))

(define (root9 x)
  (fixed-point ((repeated average-damp 2)
                (lambda (y) (/ x  (expt y 8))))
  1.0))

(define (root15 x)
  (fixed-point ((repeated average-damp 3)
                (lambda (y) (/ x  (expt y 14))))
  1.0))

; doesn't terminate with 3 repeats
(define (root16 x)
  (fixed-point ((repeated average-damp 4)
                (lambda (y) (/ x  (expt y 15))))
  1.0))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (lambda (y) (/ x  (expt y (- n 1)))))
  1.0))

(nth-root 16 1024)
(root16 1024)