#lang sicp
(#%require (only racket random))

(define (square x)
  (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral P x1 y1 x2 y2 trials)
  (let* ((area (* (abs (- x1 x2)) (abs (- y1 y2))))
         (proportion (monte-carlo trials (lambda ()
                                           (P (random-in-range x1 x2)
                                              (random-in-range y1 y2)))))
         (integral (* area proportion)))
      integral))

; area is pi*r*r. r is 3
(/ (estimate-integral (lambda (x y) (<= (+ (square (- x 5)) (square (- y 7))) (square 3))) 2 4 8 10 100000) 9.0)