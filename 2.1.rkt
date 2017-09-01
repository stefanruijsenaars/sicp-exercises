#lang sicp
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let* ((g (gcd n d))
         (num (/ n g))
         (denom (/ d g)))
    (cons (if (or (and (> num 0) (< denom 0))
                  (and (< num 0) (> denom 0)))
              (- (abs num))
              (+ (abs num)))
          (abs denom))))

(display (make-rat 1 2))
(display (make-rat (- 1) 2))
(display (make-rat (- 1) (- 2)))
(display (make-rat 1 (- 2)))