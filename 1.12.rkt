#lang sicp
(define (pascal row col)
  (cond ((= row col) 1)
        ((= col 1) 1)
        (else (+ (pascal (- row 1) (- col 1))
             (pascal (- row 1) col)))))

; example (row, col)
;        (1,1)
;     (2,1) (2,2)
;   (3,1) (3,2) (3,)