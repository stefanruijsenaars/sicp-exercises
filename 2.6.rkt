#lang sicp

(define zero
  (lambda (f)
    (lambda (x) x)))

(define add-1
  (lambda (church-n)
    (lambda (f)
      (lambda (x)
        (f ((church-n f) x))))))

; by substitution of (add-1 zero)
(define one
  (lambda (f)
    (lambda (x)
      (f x))))

; by substitution of (add-1 one)
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (inc x)
  (+ x 1))

((zero inc) 0)
((one inc) 0)
((two inc) 0)
(((add-1 two) inc) 0)

(define (+-church church-n church-m)
  (lambda (f)
    (lambda (x)
      ((church-m f) ((church-n f) x)))))

(define three (+-church one two))
((three inc) 0)
(define six (+-church three three))
((six inc) 0)