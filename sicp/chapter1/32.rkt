#lang sicp

(#%require rackunit)

; DEPS

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

(define (cube x) (* x x x))

; SOLUTION

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))
    ))
    (iter a null-value)
)

(define (sum term a next b) (accumulate + 0 term a next b))
(define (product term a next b) (accumulate * 1 term a next b))

; TESTS

(check-equal? (accumulate * 1 square 1 inc 3) 36)
(check-equal? (accumulate * 1 identity 3 inc 5) 60)
(check-equal? (accumulate + 0 identity 1 inc 10) 55)
(check-equal? (sum cube 1 inc 10) 3025)
(check-equal? (product square 1 inc 3) 36)
