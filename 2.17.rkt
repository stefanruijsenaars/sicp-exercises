#lang sicp

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(last-pair (list 23 27 149 34))
