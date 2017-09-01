#lang sicp

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (define (endpoint-sign i)
    (cond ((and (>= (upper-bound i) 0)
                (>= (lower-bound i) 0))
                ; both are positive
                1)
           ((and (< (lower-bound i) 0)
                 (< (lower-bound i) 0))
            ; both are negative
            -1)
            ; opposite signs
            (else 0)))

                     
  (let ((es-x (endpoint-sign x))
        (es-y (endpoint-sign y))
        (x-up (upper-bound x))
        (x-lo (lower-bound x))
        (y-up (upper-bound y))
        (y-lo (lower-bound y)))
  (cond ((> es-x 0) ; both x endpoints positive or zero
         (cond ((> es-y 0)
                (make-interval (* x-lo y-lo) (* x-up y-up)))
               ((< es-y 0)
                (make-interval (* x-up y-lo) (* x-lo y-up)))
               (else
                (make-interval (* x-up y-lo) (* x-up y-up)))))
        ((< es-x 0) ; both x endpoints negative
         (cond ((> es-y 0)
                (make-interval (* x-lo y-up) (* x-up y-lo)))
               ((< es-y 0) 
                   (make-interval (* x-up y-up) (* x-lo y-lo)))
                  (else
                   (make-interval (* x-lo y-up) (* x-lo y-lo)))))
        
        (else ; opposite signs for x endpoints
          (cond ((> es-y 0)
                   (make-interval (* x-lo y-up) (* x-up y-up)))
                  ((< es-y 0)
                   (make-interval (* x-up y-lo) (* x-lo y-lo)))
                  (else ; both x and y have oppostive signs for their endpoints -> need to check values
                   (make-interval (min (* x-lo y-up) (* x-up y-lo)) 
                                  (max (* x-lo y-lo) (* x-up y-up)))))))))
                
(define (make-interval a b) (cons a b))

(define (upper-bound p)
  (cdr p))

(define (lower-bound p)
  (car p))

; (div-interval (make-interval 1 2) (make-interval 3 3))