#lang sicp


(define (make-accumulator initial)
  (let ((value initial))
    (lambda (increment)
      (begin
        (set! value (+ value increment))
        value))))

(define A (make-accumulator 5))

    
