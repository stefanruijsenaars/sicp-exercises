#lang sicp

(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else (error "unknown op:" op))))
  dispatch)

(define data (make-from-mag-ang 1 2))
(data 'magnitude)
(data 'angle)
(data 'err)