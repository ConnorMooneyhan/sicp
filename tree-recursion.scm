(define (f n) (if (< n 3)
                  n
                  (f-iter 0 1 2 3 n)))
(define (f-iter fn-3 fn-2 fn-1 current target)
        (if (= current target)
            (+ fn-1 (* 2 fn-2) (* 3 fn-3))
            (f-iter fn-2 fn-1 (+ fn-1 (* 2 fn-2) (* 3 fn-3)) (+ current 1) target)))
