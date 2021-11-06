#lang sicp

(#%require rackunit)

; DEPS

(define (inc n) (+ n 1))

(define (square x) (* x x))

(define (identity x) x)

; SOLUTION

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))
  ))
  (iter a 1)
)


(define (factorial n) 
  (product identity 1 inc n)
)

(define (even n) (
  (= (remainder n 2) 0)
))

(define (calc-pi n)
  (define (term1 x) (
    if (even? x)
    x
    (+ x 1)))

  (define (term2 x) (
    if (even? x) 
    (- x 1)
    x))

  ( * (
      /
      (product term1 2 inc (+ n 2))
      (product term2 3 inc (+ n 3))
    ) 
  4)
)

; TESTS

(check-equal? (product square 1 inc 3) 36)
(check-equal? (product identity 3 inc 5) 60)
(check-equal? (factorial 5) 120)
(check-equal? (floor (* (calc-pi 3000) 1000)) 3141)


