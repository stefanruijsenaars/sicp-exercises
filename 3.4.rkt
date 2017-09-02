#lang sicp

(define (make-account balance secret-password)
  (let ((password secret-password)
        (bad-consecutive-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          (display "Insufficient funds")))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pw m)
      (if (eq? pw password)
          (begin
            (set! bad-consecutive-attempts 0)
            (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
          (begin
            (set! bad-consecutive-attempts (+ 1 bad-consecutive-attempts))
            (if (>= bad-consecutive-attempts 7)
                (lambda (x) (call-the-cops))
                (lambda (x)
                  "Incorrect password"
                  )))))
    dispatch))

(define acc (make-account 100 'secret-password))

(define call-the-cops (lambda () "Calling the cops"))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'some-other-password 'deposit) 50)
((acc 'secret-password 'withdraw) 50)
((acc 'some-other-password 'deposit) 50)