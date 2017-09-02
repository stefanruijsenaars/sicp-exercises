#lang sicp


(define previous #f)

(define (f x)
  (if (not previous)
      (begin (set! previous x) 0)
      previous))


(+ (f 0) (f 1))
(set! previous #f)
(+ (f 1) (f 0))