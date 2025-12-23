(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))

(define (f n)  
  (define (iter fn-1 fn-2 fn-3 count)
    (if (= n count)
        fn-1
        (iter (+ fn-1
                 (* 2 fn-2)
                 (* 3 fn-3))
              fn-1
              fn-2
              (+ count 1))))
  (if (< n 3)
      n
      (iter 2 1 0 2)))
