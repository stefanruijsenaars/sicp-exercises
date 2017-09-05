#lang sicp

; tortoise & hare algorithm

(define (list-has-cycle? lst)
  (define (step tortoise-pointer hare-pointer)
    (cond ((eq? hare-pointer nil) #f)
          ((eq? (cdr hare-pointer) nil) #f)
          ((eq? tortoise-pointer hare-pointer) #t)
          (else (step (cdr tortoise-pointer) (cddr hare-pointer)))))
  (step (cdr lst) lst))


(define lst (list 'a 'b 'c))
(list-has-cycle? lst)
(set-cdr! (cddr lst) lst)
(list-has-cycle? lst)