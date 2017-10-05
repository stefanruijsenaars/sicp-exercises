#lang racket

(require (prefix-in strm: racket/stream))

(define-syntax cons-stream
  (syntax-rules ()
	((_ a b) (strm:stream-cons a b))))
(define stream-car strm:stream-first)
(define stream-cdr strm:stream-rest)
(define stream-null? strm:stream-empty?)
(define the-empty-stream strm:empty-stream)

;; form ex 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
	  the-empty-stream
	  (cons-stream
	   (apply proc (map stream-car argstreams))
	   (apply stream-map
			  (cons proc (map stream-cdr argstreams))))))

(define (list->stream sequence)
  (if (null? sequence)
	  the-empty-stream
	  (cons-stream (car sequence)
				   (list->stream (cdr sequence)))))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(provide (all-defined-out))

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

;(define (integral delayed-integrand initial-value dt)
;  (define int
;	(cons-stream initial-value
;				 (let ((integrand (force delayed-integrand)))
;				   (add-streams (scale-stream integrand dt)
;								int))))
;  int)

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt)))))

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; made up arguments, and did not test if this solution is correct
; 10.00993499848961
(stream-ref (solve-2nd (lambda (dy y) (+ (* 1 dy) (* 20 y))) .0001 10 .0001) 100)

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ (- 1) C)))
    (define diL (add-streams (scale-stream vC (/ 1 L)) (scale-stream iL (/ (- R) L))))
    (define (stream-zip s1 s2)
      (cons-stream (cons (stream-car s1) (stream-car s2)) (stream-zip (stream-cdr s1) (stream-cdr s2))))
    (stream-zip vC iL)
    ))

(define solution ((RLC 1 1 0.2 0.1) 10 0))

(stream-ref solution 0)
(stream-ref solution 1)
(stream-ref solution 2)
(stream-ref solution 3)