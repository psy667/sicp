#lang sicp

(#%require rackunit)

; DEPS

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

; SOLUTION

(define (filtered-accumulate combiner null-value term a next b condition)
    (define (iter a result)
        (if (> a b)
            result
            (
                if (condition a) (
                    iter (next a) (combiner result (term a))
                ) (
                    iter (next a) result
                )

            )
    ))
    (iter a null-value)
)

; TESTS
(check-equal? (filtered-accumulate * 1 square 1 inc 3 odd?) 9)
(check-equal? (filtered-accumulate * 1 identity 3 inc 5 odd?) 15)
(check-equal? (filtered-accumulate + 0 identity 1 inc 10 odd?) 25)