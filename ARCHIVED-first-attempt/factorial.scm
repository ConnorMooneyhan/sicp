(define (<= x y) (or (< x y) (= x y)))
(define (! x)
  (if (= x 1)
    1
    (* x (! (- x 1)))))
