; utilities
(define (cube x) (* x x x))
(define (avg x y) (/ (+ x y) 2))

; helpers
(define tolerance 0.001)
(define (improve guess x) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (good-enough? guess prev-guess) (< (abs (- guess prev-guess)) tolerance))

; cbrt
(define (cbrt-iter guess prev-guess x) (if (good-enough? guess prev-guess)
                                           guess
                                           (cbrt-iter (improve guess x) guess x)))
(define (cbrt x) (cbrt-iter 1.0 0 x))
