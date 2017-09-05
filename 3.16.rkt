#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; 3 pairs
(define test1 (list 'a 'b 'c))

; returns 3
(count-pairs test1)


; set the car of the second pair to to the third pair (instead of the symbol b)
(set-car! (cdr test1) (cddr test1))
; returns 4
(count-pairs test1)

; now also point the car of the first pair to the second pair (instead of the symbol a)
(set-car! test1 (cdr test1))
; returns 7
(count-pairs test1)
; this would never return
;(define test2 (list 'a 'b 'c))
; set-cdr! (cddr test2) test2)

;(count-pairs test2)