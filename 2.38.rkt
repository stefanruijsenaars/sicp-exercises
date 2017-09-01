#lang sicp

(define (accumulate f acc list)
  (if (null? list)
      acc
      (f (car list)
         (accumulate f acc (cdr list)))))

(define (accumulate-n f acc seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate f acc (map car seqs))
            (accumulate-n f acc (map cdr seqs)))))


(define matrix (list (list 1 2 3 4) (list 4 5 6 7) (list 6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(display (transpose matrix))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(newline)
(display (matrix-*-matrix matrix (list (list 1) (list 2) (list 3) (list 4))))