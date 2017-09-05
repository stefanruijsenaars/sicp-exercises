#lang sicp

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-ptr deque)))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

(define (make-record item prev)
  (cons item prev))

(define (set-prev! record item)
  (set-cdr! record item))

(define (record-item record)
  (car record))

(define (record-prev record)
  (cdr record))

; inserts item at the rear
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (make-record item '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           ; set the "previous" pointer of the newly inserted pair to the current rear pointer
           (set-prev! (car new-pair) (rear-ptr deque))
           ; insert the new pair
           (set-cdr! (rear-ptr deque) new-pair)
           ; update the rear pointer
           (set-rear-ptr! deque new-pair)
           deque))))

; inserts item at the front
(define (front-insert-deque! deque item)
  (let ((new-pair (cons (make-record item  '()) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
            ; set the next value of the newly inserted pair to the current front pointer
           (set-cdr! new-pair (front-ptr deque))
           ; set the "previous" pointer of the current front pointer to the newly inserted pair
           (set-prev! (car (front-ptr deque)) new-pair)
           ; update the front pointer to the newly inserted pair
           (set-front-ptr! deque new-pair)
           deque))))

; deletes item at the front
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ; if there's only one item, update the front and rear pointers.
        ((eq? (front-ptr deque) (rear-ptr deque)) (set-front-ptr! deque '())
                                                  (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         ; set the previous pointer of the new front pointer to the empty list
         (set-prev! (car (front-ptr deque)) '())
         deque)))

;deletes item at the rear
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ; if there's only one item, update the front and rear pointers.
        ((eq? (front-ptr deque) (rear-ptr deque)) (set-front-ptr! deque '())
                                                  (set-rear-ptr! deque '()))
        (else ; update the rear pointer
              (set-rear-ptr! deque (record-prev (car (rear-ptr deque))))
              deque)))


(define (print-deque q)
  (define (iter pointer)
    (cond ((null? pointer) (display "EMPTY deque!") (newline))
          ; stop iterating when the pointer reaches the rear pointer
          ((eq? pointer (rear-ptr q)) (display (record-item (car pointer))) (newline))
          (else (display (record-item (car pointer))) (newline) (iter (cdr pointer)))))
  (display "deque:")
  (newline)
  (iter (front-ptr q)))

(define d1 (make-deque))
(front-insert-deque! d1 'a)
(front-insert-deque! d1 'b)
(print-deque d1)
(rear-delete-deque! d1)
(print-deque d1)
(rear-delete-deque! d1)
(print-deque d1)
(rear-insert-deque! d1 'a)
(rear-insert-deque! d1 'b)
(front-delete-deque! d1)
(print-deque d1)
(front-delete-deque! d1)
(print-deque d1)
