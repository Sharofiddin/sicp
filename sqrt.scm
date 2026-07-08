(define (sqrt-iter guess x)
  (if (good-enough? guess x) 
    guess 
    (sqrt-iter (improve guess x) x)))

(define (sqrt-iter-imp guess old-guess x)
  (if (good-enough-from-old? guess old-guess) 
    guess 
    (sqrt-iter-imp (improve guess x) guess x)))
(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x) 
    guess 
    (sqrt-iter-new-if (improve guess x) x)))

(define (improve guess x)
 (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)

(define (good-enough-from-old? guess old-guess)
  (< (abs (/ (- guess old-guess) guess)) 0.000000001)
  )

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-new x)
  (sqrt-iter-new-if 1.0 x))

(define (sqrt-imp x)
  (sqrt-iter-imp 1.0 0.0 x))

(define (cube-root-iter guess old-guess x)
  (if (good-enough-from-old? guess old-guess)
    guess
    (cube-root-iter (improve-cube guess x) guess x)))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3)
  )

(define (croot x)
  (cube-root-iter 1.0 0.0 x)
  )
