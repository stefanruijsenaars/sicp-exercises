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

(define (sign-change-detector x y)
  (cond ((and (< x 0) (> y 0)) -1)
        ((and (> x 0) (< y 0)) 1)
        (else 0)))

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings (stream-cdr input-stream)
                          (stream-car input-stream)
                          avpt))))

(define sense-data (cons-stream 1 (cons-stream 2 (cons-stream -1 (cons-stream -2 (cons-stream 0.4 the-empty-stream))))))

(define zero-crossings (make-zero-crossings sense-data 0 0))


; 0
; 0
; 0
; -1
; 0
(stream-ref zero-crossings 0)
(stream-ref zero-crossings 1)
(stream-ref zero-crossings 2)
(stream-ref zero-crossings 3)
(stream-ref zero-crossings 4)