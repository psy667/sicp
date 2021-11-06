#lang sicp

(#%require rackunit)

; DEPS

(define (cube x) (* x x x))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0)
)

; SOLUTION

(define (simpson f a b n)
  (define (h)
    (/ (- b a) n))
  
  (define (y k)
    (f (* (+ a k) (h))))

  (define (term k)
    (if (= (remainder k 2) 0)
      (* 2 (y k))
      (* 4 (y k))))

  ( * (sum term a inc n)
      (/ (h) 3.0)))

; TESTS

(check-equal? (round (* 100 (simpson cube 0 1 100))) 25.0)
(check-equal? (round (* 100 (simpson cube 0 1 1000))) 25.0)
(check-equal? (floor (* 1000 (simpson cube 0 1 100))) 253.0)
(check-equal? (floor (* 1000 (simpson cube 0 1 1000))) 250.0)