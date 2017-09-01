#lang sicp

(define (sum term a next b) 
   (if (> a b) 
       0 
       (+ (term a) 
          (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;(integral cube 0 1 0.01)

(define (increase x)
  (+ x 1))

(define (integral-simpsons f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (get-term k) (*
                 (cond ((or (= k 0) (= k n)) 1)
                       ((odd? k) 4)
                       (else 2))
                 (y k)))
  (* (/ h 3) (sum get-term 0 increase n)))

(integral-simpsons cube 0 1 100)
(integral-simpsons cube 0 1 1000)