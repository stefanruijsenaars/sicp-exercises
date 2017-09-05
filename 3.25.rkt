#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup key-list)
      (define (lookup-iter key-list item)
        ; find out if we're at the deepest level of the key-list
        (cond ((null? (cdr key-list))
               (let ((record (assoc (car key-list) (cdr item))))
                 (if record
                     (cdr record)
                     #f)))
              (else (let ((subtable (assoc (car key-list) (cdr item))))
                      (if subtable
                          (lookup-iter (cdr key-list) subtable)
                          #f)))))
      (lookup-iter key-list local-table))
    
    (define (insert! key-list value)
      ; item is the subtable we're inserting in to.
      (define (insert-iter! key-list item value)
        ; find out if we're at the deepest level of the key-list
        (cond ((null? (cdr key-list))
               ; insert the value at the deepest level
               (let ((record (assoc (car key-list) (cdr item))))
                 (if record
                     (set-cdr! record value)
                     (set-cdr! item
                               (cons (cons (car key-list) value)
                                     (cdr item))))))
              ; find out if the subtable exists
              (else (let ((subtable (assoc (car key-list) (cdr item))))
                      (if (not subtable)
                        ; create the subtable
                        (set-cdr! item
                                  (cons (list (car key-list))
                                   (cdr item)))
                        ; do nothing
                        (values))
                      ; iterate a level deeper
                      (let ((new-subtable (assoc (car key-list) (cdr item))))
                        (insert-iter! (cdr key-list) new-subtable value)))))
      )
      (insert-iter! key-list local-table value)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup key-list table)
  ((table 'lookup-proc) key-list))

(define (insert! key-list value table)
  ((table 'insert-proc!) key-list value))

(define t (make-table))

(insert! (list 'b) 'cats t)
(insert! (list 'a 'b 'c) 'dogs t)
(insert! (list 'b) 'cats t)

(lookup (list 'a 'b 'c) t)
(lookup (list 'b) t)

(lookup (list 'a 'c) t)