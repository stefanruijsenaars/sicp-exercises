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

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (log-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (log-summands (+ n 1)))))

(define (partial-sums s) 
   (add-streams s (cons-stream 0 (partial-sums s))))

(define log-stream
  (partial-sums (log-summands 1)))


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


; 0.693 after many many iterations
;(display-stream log-stream)

; 9.693 after 5 iterations
;(display-stream (euler-transform log-stream))

; 9.693 after 3 iterations
;(display-stream (accelerated-sequence euler-transform log-stream))