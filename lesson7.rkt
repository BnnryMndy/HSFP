#lang scheme

(display "lesson 7") (newline) (newline)

(define a^xn
  (lambda (p x)
    (define tf
      (lambda (p x summ x^n)
        (if (null? p)
            summ
            (tf (cdr p) x (+ summ (*(car p) x^n)) (* x^n x))
            )
        )
      )
    (tf p x 0 1)
    )
   )

(a^xn (list 1 2 1) 2)

(define dp
  (lambda (p)
    (define dp1
      (lambda (p n)
          (if(null? p)
             null
             (cons (* n (car p)) (dp1 (cdr p) (+ n 1)))
             )
        )
      )
    
    (dp1 (cdr p) 1)
  ))
(dp (list 1 2 1 2))


(define p+p
  (lambda (p1 p2)
    (if (null? p1)
        p2
        (if (null? p2)
            p1
            (cons (+ (car p1) (car p2))
           (p+p (cdr p1) (cdr p2))
           ))
     )
    )
  )

(p+p (list 1 2 3) (list 1 2 3 4 5))



(define p*p
  (lambda (p1 p2)
    ()
    )
  )






(define p*b
  (lambda (p b)
    (if (null? p)
        null
        (cons (* (car p) b) (p*b (cdr p) b))
        )
    )
  )
