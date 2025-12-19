; utilities
(define (avg x y)
  (/ (+ x y) 2))
(define (square x) (* x x))
(define (abs x)
  (if (< x 0)
      (- x)
      x))

; helpers
(define tolerance 0.0001)
(define (good-enough? guess prev-guess) (< (abs (- guess prev-guess)) tolerance))
(define (improve guess x) (avg guess (/ x guess)))

; sqrt
(define (sqrt-iter guess prev-guess x) (if (good-enough? guess prev-guess)
                                           guess
                                           (sqrt-iter (improve guess x) guess x)))
(define (my-sqrt x) (sqrt-iter 1.0 -1000000 x))
