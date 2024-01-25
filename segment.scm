(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment))
        (avg (lambda (a b) (/ (+ a b) 2))))
    (make-point (avg (x-point start) (x-point end))
                (avg (y-point start) (y-point end)))))
