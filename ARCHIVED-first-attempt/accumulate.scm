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
