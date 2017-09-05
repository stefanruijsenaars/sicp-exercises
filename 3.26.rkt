#lang sicp

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

; every entry has a key and a value
(define (entry-key entry) (car entry))
(define (entry-value entry) (cdr entry))

(define (set-value! entry value)
  (set-cdr! entry value))

(define (make-entry key value)
  (cons key value))

(define (make-tree entry left right)
  (list entry left right))

; sorted by (numeric) key
(define (adjoin-set key value set)
  (cond ((null? set) (make-tree (make-entry key value) '() '()))
        ((= key (entry-key (entry set))) (begin
                                           (set-value! (entry set) value)
                                           set))
        ((< key (entry-key (entry set)))
         (make-tree (entry set)
                    (adjoin-set key value (left-branch set))
                    (right-branch set)))
        ((> key (entry-key (entry set)))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set key value (right-branch set))))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (entry-key (entry set-of-records))) (entry set-of-records))
        ((< given-key (entry-key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup-in-table key)
      (lookup key (cdr local-table)))
    (define (insert! key value)
      (set-cdr! local-table (adjoin-set key value (cdr local-table))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup-in-table)
            ((eq? m 'insert-proc!) insert!)
            (else (error "invalid"))))
    dispatch))

(define t (make-table))

; insert time: O(log n)
((t 'insert-proc!) 1 8)
((t 'insert-proc!) 4 9)
((t 'insert-proc!) 2 10)
((t 'insert-proc!) 5 11)
((t 'insert-proc!) 3 12)

; lookup time: O(log n)
((t 'lookup-proc) 1)
((t 'lookup-proc) 4)
((t 'lookup-proc) 2)
((t 'lookup-proc) 5)
((t 'lookup-proc) 3)
