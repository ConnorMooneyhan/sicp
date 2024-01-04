(define (recursive-sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (recursive-sum term (next a) next b))))

(define (recursive-product term a next b)
  (if (> a b)
      1
      (* (term a)
         (recursive-product term (next a) next b))))

(define (recursive-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (recursive-accumulate combiner null-value term (next a) next b))))

(define (rec-acc-sum term a next b)
  (recursive-accumulate + 0 term a next b))

(define (rec-acc-product term a next b)
  (recursive-accumulate * 1 term a next b))

(define (iterative-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (iterative-product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (iterative-accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (iter-acc-sum term a next b)
  (iterative-accumulate + 0 term a next b))

(define (iter-acc-product term a next b)
  (iterative-accumulate * 1 term a next b))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (filter a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (and (> n 1) (= n (smallest-divisor n))))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (id x) x)

(define (sum-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (relatively-prime? n k)
  (define lowest (if (< n k) n k))
  (define (iter current)
    (if (> current lowest)
        #t
        (if (and (divides? current n)
                 (divides? current k))
            #f
            (iter (+ current 1)))))
  (iter 2))

(define (multiply-relative-primes n)
  (define (test? x) (relatively-prime? x n))
  (filtered-accumulate * 1 id 2 inc (- n 1) test?))
