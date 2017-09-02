#lang sicp

(define (make-account balance secret-password)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pw m)
      (if (eq? pw secret-password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))
      "Incorrect password"))
    dispatch)

(define peter-acc (make-account 100 'open-sesame))

(define (make-joint other-acc other-password new-password)
  (define (dispatch pw m)
    (if (eq? pw new-password)
        (other-acc other-password m)
        "Incorrect password"))
  dispatch)

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'withdraw) 3)
((peter-acc 'open-sesame 'deposit) 6)
