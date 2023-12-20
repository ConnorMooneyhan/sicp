(define (square n) (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time))))

; my work

(define (search-for-primes-iter current upper-bound)
  (timed-prime-test current)
  (define next (+ current 2))
  (if (not (> next upper-bound))
    (search-for-primes-iter next upper-bound)))

(define (search-for-primes lower-bound upper-bound)
  (if (even? lower-bound)
    (search-for-primes-iter (+ 1 lower-bound) upper-bound)
    (search-for-primes-iter lower-bound upper-bound)))
