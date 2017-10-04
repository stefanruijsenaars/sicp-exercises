#lang sicp

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

(define mapped (stream-map (lambda (x y) (+ x y)) (stream-enumerate-interval 1 10) (stream-enumerate-interval 2 20)))

;3
(stream-car mapped)
;5
(stream-car (stream-cdr mapped))
;7
(stream-car (stream-cdr (stream-cdr mapped)))
