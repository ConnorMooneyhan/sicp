(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (define new-n (/ n g))
    (define new-d (/ d g))
    (if (< new-d 0)
      (cons (- new-n) (- new-d))
      (cons new-n new-d))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
