; segment.scm
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

; new stuff
(define (length segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (sqrt (+ (square (- (x-point start) (x-point end)))
             (square (- (y-point start) (y-point end)))))))

(define (make-rectangle bl br tr tl)
  (cons (cons bl br)
        (cons tl tr)))

(define (top rectangle)
  (make-segment (car (cdr rectangle))
                (cdr (cdr rectangle))))

(define (left rectangle)
  (make-segment (car (car rectangle))
                (car (cdr rectangle))))

(define (bottom rectangle)
  (make-segment (car (car rectangle))
                (cdr (car rectangle))))

(define (right rectangle)
  (make-segment (cdr (car rectangle))
                (cdr (cdr rectangle))))

(define (perimeter rectangle)
  (+ (length (top rectangle))
     (length (left rectangle))
     (length (bottom rectangle))
     (length (right rectangle))))

(define (area rectangle)
  (* (length (left rectangle))
     (length (top rectangle))))
