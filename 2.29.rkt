#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  ; cadr because it's a list, not a pair
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  ; cadr because it's a list, not a pair
  (car (cdr branch)))

(define (is-weight? branch)
  (number? branch))

(define (total-weight mobile)
  (let ((left (branch-structure (left-branch mobile)))
       (right (branch-structure (right-branch mobile))))
    (+ (if (is-weight? left)
         left
         (total-weight left))
       (if (is-weight? right)
         right
         (total-weight right)))))

(define (branch-torque branch)
  (if (is-weight? (branch-structure branch))
      (* (branch-structure branch) (branch-length branch))
      (* (total-weight (branch-structure branch)) (branch-length branch))))

(define (mobile-is-balanced mobile)
  (let ((left (branch-structure (left-branch mobile)))
       (right (branch-structure (right-branch mobile))))
    (and (= (branch-torque (left-branch mobile))
            (branch-torque (right-branch mobile)))
       (if (is-weight? left)
           #t
           (mobile-is-balanced left))
       (if (is-weight? right)
           #t
           (mobile-is-balanced right)))))

(define mob (make-mobile (make-branch 1 (make-mobile (make-branch 1 20) (make-branch 1 30))) (make-branch 1 40)))

(define mob1 (make-mobile (make-branch 1 (make-mobile (make-branch 1 20) (make-branch 1 20))) (make-branch 1 40)))

(total-weight mob)
(mobile-is-balanced mob)
(mobile-is-balanced mob1)