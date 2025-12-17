(define (cont-frac-recursive n d k)
  (define (rest current)
   (if (> current k)
     0
     (/ (n current)
        (+ (d current) (rest (+ current 1))))))
  (rest 1))

(define (cont-frac-iter n d k)
  (define (iter current acc)
    (if (> current k)
        acc
        (iter (+ current 1) (/ (n current)
                               (+ (d current) acc)))))
  (iter 1 0))
