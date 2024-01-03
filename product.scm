(define (recursive-product term a next b)
  (if (> a b)
    1
    (* (term a)
       (recursive-product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (id x) x)

(define (inc x) (+ x 1))

(define (add-two x) (+ x 2))

(define (factorial x)
  (product id 1 inc x))

(define (square x) (* x x))

(define (pi terms)
  (define (term x) (/ (* x (+ x 2)) (square (+ x 1))))
  (define (next x) (+ x 2))
  (* 4 (product term 2.0 next terms)))
