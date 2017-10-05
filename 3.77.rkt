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

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

