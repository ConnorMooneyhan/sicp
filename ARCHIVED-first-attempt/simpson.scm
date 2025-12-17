(define (add-two n) (+ n 2))

(define (better-sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term num) (+ (* 4 (y num)) (* 2 (y (+ num 1)))))
  (*
    (/ h 3.0)
    (+ (y 0)
       (sum term 1 add-two (- n 1))
       (y n))))

(define (better-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term num) (+ (* 4 (y num)) (* 2 (y (+ num 1)))))
  (*
    (/ h 3.0)
    (+ (y 0)
       (better-sum term 1 add-two (- n 1))
       (y n))))
