#lang sicp
(#%require (only racket random))

(define internal 0)

(define (rand-update x) (+ x 1))

(define (rand m)
  (define dispatch
    (cond ((eq? m 'reset) (lambda (x) (set! internal x)))
            ((eq? m 'generate) (begin (set! internal (rand-update internal)) internal))
            (else (error "Invalid command"))))
    dispatch)