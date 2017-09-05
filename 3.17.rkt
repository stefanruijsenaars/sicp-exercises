#lang sicp

(define (count-pairs x)
  (define pairs-seen (list 'table))

  (define (last-pair x)
    (if (null? (cdr x))
        x
        (last-pair (cdr x))))

  (define (add-to-pairs-seen item)
    (set-cdr! (last-pair pairs-seen) (cons item nil)))

   
  (define (item-in-pairs-seen? item)
    (define (iter pairs)
      (cond ((null? pairs) #f)
            ((eq? (car pairs) item) #t)
            (else (iter (cdr pairs)))))
    (iter pairs-seen))
  
  (define (count-it x)
    (cond ((not (pair? x)) 0)
          ((item-in-pairs-seen? x) 0)
          (else (add-to-pairs-seen x)
                (+ (count-it (car x))
                   (count-it (cdr x))
                   1))))
  (count-it x))
     

; 3 pairs
(define test1 (list 'a 'b 'c))

; returns 3
(count-pairs test1)


; set the car of the second pair to to the third pair (instead of the symbol b)
(set-car! (cdr test1) (cddr test1))
; returns 4
(count-pairs test1)


; now also point the car of the first pair to the second pair (instead of the symbol a)
(set-car! test1 (cdr test1))
; returns 7
(count-pairs test1)

; this would never return
;(define test2 (list 'a 'b 'c))
; set-cdr! (cddr test2) test2)

;(count-pairs test2)