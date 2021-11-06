#lang sicp

(#%require rackunit)

; DEPS

(define (inc n) (+ n 1))

(define (cube x) (* x x x))

(define (identity x) x)

; SOLUTION

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0)
)

; TESTS

(check-equal? (sum cube 0 inc 2) 9)
(check-equal? (sum cube 1 inc 10) 3025)
(check-equal? (sum identity 1 inc 10) 55)