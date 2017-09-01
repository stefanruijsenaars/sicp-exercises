#lang sicp

(define (cont-frac n d k)
  (define (do-it i)
    (/ (n i)
       (if (= i k)
         (d i)
         (+ (d i) (do-it (+ i 1))))))
  (do-it 1))

;should be: 0.61803

;0.625
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)
;0.6180257510729613
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)


(define (cont-frac-iter n d k)
  (define (do-it result cur)
    (if (= cur 0)
        result
        (do-it (/ (n cur) (+ (d cur) result)) (- cur 1))))
        
  (do-it 0 k))

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 5)
;0.625
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 12)
;0.6180257510729613