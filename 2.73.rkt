#lang sicp

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? x y) (and (variable? x) (variable? y) (eq? x y)))

(define (apply-generic op . args) 
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define table (list))

(define (put op type proc)
  (set! table (append table (list (list op type proc)))))

(define (get op type)
  (define (search op type t)
    (cond ((null? t) #f)
          ((and (eqv? (caar t) op) (eqv? (cadar t) type))
           (caddar t))
          (else (search op type (cdr t)))))
  (search op type table))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (content datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))


(define (install-sum-package)
  (define (make-sum a b)
    (cond ((eq? a 0) b)
          ((eq? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  
  (define (deriv-sum s var)
    (make-sum (deriv (addend s) var)
              (deriv (augend s) var)))
  (put 'deriv '+ deriv-sum))

(define (install-product-package)
  (define (multiplier s) (car s))
  (define (multiplicand s) (cadr s)) 
  (define (make-product a b)
    (cond ((or (eq? a 0) (eq? b 0)) 0)
          ((eq? a 1) b)
          ((eq? b 1) a)
          ((and (number? a) (number? b)) (* a b))
          (else (list '* a b))))
  (define (make-sum a b)
    (cond ((eq? a 0) b)
          ((eq? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b))))
  (define (deriv-product p var)
    (make-sum
     (make-product (multiplier p)
                  (deriv (multiplicand p) var))
     (make-product (deriv (multiplier p) var)
                   (multiplicand p))))
  (put 'deriv '* deriv-product))

(define (install-exponent-package)
  (define (make-product a b)
    (cond ((or (eq? a 0) (eq? b 0)) 0)
          ((eq? a 1) b)
          ((eq? b 1) a)
          ((and (number? a) (number? b)) (* a b))
          (else (list '* a b))))
  (define (exponent expr)
    (cadr expr))
  (define (base expr)
    (car expr))
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((=number? base 1) 1)
          (else (list '** base exponent))))
  (define (deriv-exponent exp var)
    (make-product (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))) (deriv (base exp) var)))
  (put 'deriv '** deriv-exponent))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(install-sum-package)
(install-product-package)
(install-exponent-package)
(get 'deriv '+)

(deriv '(+ x x) 'x)
(deriv '(* x y) 'x)
(display (deriv '(+ (* x 2) (* x y)) 'x))
(display (deriv '(+ (** x 3) (* x y)) 'x))
