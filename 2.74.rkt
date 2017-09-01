#lang sicp

(define (type-tag file) 
   (car file))

(define (apply-generic op name file) 
   (let ((division-name (type-tag file))) 
     (let ((proc (get op division-name))) 
       (if proc 
           (proc name (cdr file))
           (error "no result")))))

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


(define division1-file1 '(division1 (joe (address (1 street)) (salary 1000)) (mike (address (2 street)) (salary 2000))))
(define division2-file1 '(division2 ("name=don" ((3 street)) 3000) ("name=peter" ((4 street) 4000)) ("name=joe" ((1 street) 1000))))

(define (install-division1-package)
  (define (get-record employee-name file)
    (cond ((null? file) (error "no result"))
          ((eq? (car (car file)) (string->symbol employee-name)) (car file))
          (else (get-record employee-name (cdr file)))))
  (define (get-salary employee-name file)
    (let ((record (get-record employee-name file)))
      (if (null? record)
          0
          (car (cdaddr record)))))
  (put 'get-record 'division1 get-record)
  (put 'get-salary 'division1 get-salary))

  
(define (install-division2-package)
  (define (get-record employee-name file)
    (cond ((null? file) (error "no result"))
          ((equal? (car (car file)) (string-append "name=" employee-name)) (car file))
          (else (get-record employee-name (cdr file)))))
  (define (get-salary employee-name file)
    (let ((record (get-record employee-name file)))
      (if (null? record)
          (error "no result")
          (cadadr record))))
  (put 'get-record 'division2 get-record)
  (put 'get-salary 'division2 get-salary))

(install-division1-package)
(install-division2-package)

; file already contains type tags
(define (get-record employee-name file)
  (apply-generic 'get-record employee-name file))

(define (get-salary employee-name file)
  (apply-generic 'get-salary employee-name file))

(display (get-record "mike" division1-file1))
(newline)
(display (get-salary "mike" division1-file1))
(newline)
(display (get-record "peter" division2-file1))
(newline)
(display (get-salary "peter" division2-file1))
(newline)

(define (find-employee-record employee-name seq)
  (if (null? seq)
      '()
      (append (list (get-record employee-name (car seq))) (find-employee-record employee-name (cdr seq)))))

(display (find-employee-record "joe" (list division1-file1 division2-file1)))