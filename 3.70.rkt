#lang sicp

(define (display-line x)
  (newline)
  (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low 
                   (stream-enumerate-interval (+ 1 low) high))))


(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (integrate-series s)
  (stream-map / s integers))

;derivative of cosine is the negative of sine
(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2)) (mul-series s1 (stream-cdr s2)))))

(define x (mul-series cosine-series cosine-series))
(define y (mul-series sine-series sine-series))

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

(define (div-series s1 s2)
  (let ((denominator-constant (stream-car s2)))
  (if (= 0 (stream-car s2))
      (error ("cannot divide by zero"))
      (scale-stream (mul-series s1
                                (invert-unit-series
                                    ; divide by constant so that we have a unit constant term
                                    (scale-stream s2 (/ 1 denominator-constant))))
                    (/ 1 denominator-constant)))))

(define (stream-limit s tolerance)
  (if (eq? (stream-car (stream-cdr s)) the-empty-stream)
      (error "not found")
      (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) tolerance)
          (stream-car (stream-cdr s))
          (stream-limit (stream-cdr s) tolerance))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
     (pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
     (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
     (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (square x)
  (* x x))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (<= (weight s1car) (weight s2car))
               (cons-stream s1car
                            (merge-weighted (stream-cdr s1)
                                            s2
                                            weight))
               (cons-stream s2car
                            (merge-weighted s1
                                            (stream-cdr s2)
                                            weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
    (merge-weighted
     (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
     (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
     weight)))

(define (divisible? n x)
  (zero? (remainder n x)))

(define a (weighted-pairs integers integers (lambda (x) (let ((i (car x))
                                                              (j (cadr x)))
                                                          (+ i j)))))
;(display-stream a)

(define integers-not-mod-2-3-5 (stream-filter
                                (lambda (x) (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5))))
                                integers))

(define b (weighted-pairs integers-not-mod-2-3-5 integers-not-mod-2-3-5 (lambda (x)
                                                                          (let ((i (car x))
                                                                                (j (cadr x)))
                                                                            (+ (* 2 i)
                                                                               (* 3 j)
                                                                               (* 5 i j))))))
;(display-stream b)
