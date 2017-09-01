#lang sicp

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        (( divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (accumulate f acc seq)
  (if (null? seq)
      acc
      (f (car seq)
         (accumulate f acc (cdr seq)))))
      
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (append list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append (cdr list1) list2))))


(define (double x) (* x 2))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
                            
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; adjoins a new column/row position to a set of positions
(define (adjoin-position row col rest)
  (cons (new-queen row col) rest))
; represents an empty set of positions
(define empty-board nil)
; determines for a set of positions, whether the queen in the kth column is safe with respect to the others.
(define (safe? k positions)
  (accumulate (lambda (x y)
                (and x y)) #t
      (map (lambda (pos)
           (or
            (and (row-conflict (car positions) pos) (col-conflict (car positions) pos))
            (and
             (not (row-conflict (car positions) pos))
             (not (col-conflict (car positions) pos))
             (not (diagonal-conflict (car positions) pos))
            )))
         positions)))

(define (row-conflict q1 q2)
  (= (queen-row q1) (queen-row q2)))

(define (col-conflict q1 q2)
  (= (queen-col q1) (queen-col q2)))

(define (diagonal-conflict q1 q2)
  (or (= (+ (queen-row q1) (queen-col q1)) (+ (queen-row q2) (queen-col q2)))
      (= (- (queen-row q1) (queen-col q1)) (- (queen-row q2) (queen-col q2))))
  )


(define (new-queen row col)
  (cons row col))

(define (queen-row queen) 
   (car queen))

(define (queen-col queen) 
   (cdr queen))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          ; rest of queens is a way to place k - 1 queens in the first k - 1 columns
          (lambda (rest-of-queens)
            ; new row is a proposed row in which to place the queen for the kth column
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))