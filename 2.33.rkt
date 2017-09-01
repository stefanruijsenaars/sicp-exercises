#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (first acc) (cons (p first) acc)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (first acc) (+ acc 1)) 0 sequence))

(map (lambda (x) (* x x)) (list 1 2 3))
(length (list 1 2 3))
(append (list 1 2 3) (list 4 5 6))