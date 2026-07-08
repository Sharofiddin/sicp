(define (f_rec n)
  (cond 
     ((< n 3) n)
     (else (+
             (f_rec (- n 1)) 
             (* 2 (f_rec (- n 2) ))
             (* 3 (f_rec (- n 3) ))
          )
     )
  )
)

(define (f_iter n)
  (in_iter 0 n 0 1 2)
)

(define (in_iter step n val1 val2 val3)
  (cond 
    ((< n 3) n)
    ((= step (- n 2)) val3)
    (else (in_iter 
            (+ step 1) 
            n 
            val2 
            val3 
            ( + val3 (* 2 val2) (* 3 val1) )
          )
    )
    )
)
