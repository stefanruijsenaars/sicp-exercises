#lang racket/gui

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin f)
  (car f))

(define (frame-edge1 f)
  (cadr f))

(define (frame-edge2 f)
  (caddr f))

(define f (make-frame 1 2 3))
(frame-origin f)
(frame-edge1 f)
(frame-edge2 f)


(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-origin-2 f)
  (car f))

(define (frame-edge1-2 f)
  (car (cdr f)))

(define (frame-edge2-2 f)
  (cdr (cdr f)))

(define f2 (make-frame-2 1 2 3))
(frame-origin-2 f2)
(frame-edge1-2 f2)
(frame-edge2-2 f2)