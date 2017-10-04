#lang sicp

(define (ripple-carry-adder a-list b-list s-list c-out)
  (define (recurse A B S C-IN C-OUT)
    (full-adder (car A) (car B) C-IN (car S) C-OUT)
    (if (null? (cdr A))
        (set-signal! C-IN 0)
        (recurse (cdr A) (cdr B) C-OUT (make-wire)))
    )
  (recurse a-list b-list s-list (make-wire) c-out))