#lang sicp

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))



(define (square x)
  (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))    
  (put 'add '(scheme-number scheme-number)
       +)
  (put 'sub '(scheme-number scheme-number)
       -)
  (put 'mul '(scheme-number scheme-number)
       *)
  (put 'div '(scheme-number scheme-number)
       /)
  (put 'make 'scheme-number
       (lambda (x) x))
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(install-scheme-number-package)

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (term-list p) (cdr p))
  (define variable? symbol?)
  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  
  (define (add-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2)) (make-poly (variable p1)
                                                                   (add-terms (term-list p1) (term-list p2))))
           ; p1 is highest order
           ((eq? (variable p1) (highest-order-variable (variable p1) (variable p2))) (add-poly p1 (contents (coerce-poly (variable p1) (tag p2)))))
           ; p2 is highest order
           (else (add-poly (contents (coerce-poly (variable p2) (tag p1)) p2)))))
  
  (define (sub-terms L1 L2)
    (add-terms L1 (mul-terms L2 (adjoin-term (make-term 0 -1) (the-empty-termlist)))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (mul-poly p1 p2)
    (cond ((same-variable? (variable p1) (variable p2)) (make-poly (variable p1)
                                                                   (mul-terms (term-list p1) (term-list p2))))
           ; p1 is highest order
           ((eq? (variable p1) (highest-order-variable (variable p1) (variable p2))) (mul-poly p1 (contents (coerce-poly (variable p1) (tag p2)))))
           ; p2 is highest order
           (else (mul-poly (contents (coerce-poly (variable p2) (tag p1)) p2)))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (negate-poly p var)
    (mul-poly p (make-poly var (adjoin-term (make-term 0 -1) (the-empty-termlist)))))
  
  ; subtract p2 from p1
  (define (sub-poly p1 p2)
    (add-poly p1 (negate-poly p2 (variable p2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

   (define (mixed-add x p)
     (define (zero-order L)
       (let ((t1 (first-term L))) 
         (cond ((empty-termlist? L) #f)  
               ((= 0 (order t1)) t1) 
               (else  
                (zero-order (rest-terms L)))))) 
     (let ((tlst (term-list p))) 
       (let ((last-term (zero-order tlst))) 
         (if last-term 
             (make-poly (variable p) (adjoin-term 
                                      (make-term 0 
                                                 (add x (coeff last-term))) 
                                      tlst)) 
             (make-poly (variable p) (adjoin-term (make-term 0 x) tlst)))))) 
  
   (define (mixed-mul x p) 
     (make-poly (variable p) 
                (mul-term-by-all-terms (make-term 0 x) 
                                       (term-list p))))
  
  (define (tag p) (attach-tag 'polynomial p))
  (put 'mul '(scheme-number polynomial)
        (lambda (x p) (tag (mixed-mul x p))))
  (put 'add '(scheme-number polynomial)
        (lambda (x p) (tag (mixed-add x p))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (define (=zero-all-terms? L)
           (cond ((empty-termlist? L) #t)
                 ((not (=zero? (coeff (first-term L)))) #f)
                 (else (=zero-all-terms? (rest-terms L)))))
         (=zero-all-terms? (term-list p))))
  'done)
(install-polynomial-package)

(define (=zero? x)
  (if (symbol? x)
      #f
      (apply-generic '=zero? x)))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; order of the variables, from highest to lowest
(define variable-order (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p 'q 'r 's 't 'u 'v 'w 'x 'y 'z))

; returns the variable with the highest order out of two
(define (highest-order-variable var1 var2)
  (define (iter seq)
    (if (eq? (car seq) var1)
        var1
        (if (eq? (car seq) var2)
            var2
            (iter (cdr seq)))))
  (iter variable-order))

; create a polynomial in var by creating one with a single zero-order term
(define (coerce-poly var p)
  (make-polynomial var (adjoin-term (make-term 0 p) (the-empty-termlist))))

; f(x) => x^10 + 2*y^2
(define poly1 (make-polynomial 'x (adjoin-term (make-term 10 1) (adjoin-term (make-term 0 (make-polynomial 'y (adjoin-term (make-term 2 2) (the-empty-termlist)))) (the-empty-termlist)))))
(display poly1)
(newline)
; f(y) => 3y
(define poly2 (make-polynomial 'y (adjoin-term (make-term 1 3) (the-empty-termlist))))
(display poly2)
(newline)

(display (add poly1 poly2))
(newline)
(display (mul poly1 poly2))