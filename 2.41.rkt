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
  
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list j i))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(newline)

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (append j (list i)))
          (unique-pairs (- i 1))))
   (enumerate-interval 1 n)))

(define (unique-triples-with-sum-s s)
  (filter
     (lambda (triple)
       (= (accumulate + 0 triple) s)) (unique-triples (- s 3))))

(display (unique-triples 9))
(newline)
(display (unique-triples-with-sum-s 9))
(newline)