#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)

        (else (element-of-set? x (cdr set)))))

; O(n^2), as append is O(n)
(define (adjoin-set-iter x set)
  (define (iter before after)
    (let ((first (car after)))
      (cond ((null? after) (append set (list x)))
            ((= first x) set)
            ((< x first) (append before (cons x after)))
            (else (iter (append before (list (car after))) (cdr after))))
    ))
  (iter '() set))

; O(n), but half the steps
(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(adjoin-set 3 '(1 2 4))