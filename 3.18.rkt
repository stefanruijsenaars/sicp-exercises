#lang sicp

(define (list-contains x seq)
  (cond ((null? seq) #f)
        ((eq? (car seq) x) #t)
        (else (list-contains x (cdr seq)))))

          
(define (list-has-cycle? seq)
  (let ((visited '()))
    (define (recurse seq)
      (cond ((null? seq) #f)
            ((list-contains (car seq) visited) #t)
            (else (set! visited (cons (car seq) visited))
                  (recurse (cdr seq)))))
    (recurse seq)))

(define lst (list 'a 'b 'c))
(list-has-cycle? lst)
(set-cdr! (cddr lst) lst)
(list-has-cycle? lst)