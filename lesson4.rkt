#lang scheme

(display "Lesson 4")
(newline)
(newline)

(display "Производная")
(newline)
(define dx 1e-6)

(define
  df (lambda (f x)
                (/(- (f (+ x dx))
                     (f (- x dx))
                     )
                  2 dx
                  )
                )
          )

  
 
(display "sin(0)' = ")
(df sin 0.)

(display "(x^3-x)' = ")
(df (lambda (x) (- (* x x x) x)) 1)

(define eps 1e-12)

(define newton
  (lambda (f x0)
    (define newt
      (lambda (f x0 x1)
        (if (< (abs (- x0 x1)) eps)
            x1
            (newt f x1 (- x1 (/
                              (f x1)
                              (df f x1)
                              )
                        )
            )
         )
       )
     )
    (newt f (+ x0 1) x0)
   )
 )

(display "newton")
(newline)
(newton (lambda (x) (- 8 (* x x x))) 1)


(define dfn
  (lambda (f x0 n)
    (if (= n 0)
        (f x0)
        (/ (- (dfn f (+ x0 dx) (- n 1))
              (dfn f (- x0 dx) (- n 1))
           )
           2 dx
        )
    )
   )
  )

(display "dfn")
(newline)

(dfn sin 0. 9)


(define cosx
  (lambda (x)
    (define cos2
      (lambda (x n n! sum x^n pm)
        (if (< (abs (/ x^n n!)) eps)
            sum
            (cos2 x (+ n 2) (* n! (+ n 1) (+ n 2))
                  (+ sum (* pm (/ x^n n!)))
                  (* x^n x x)
                  (- pm)
            )
        )
       )
     )
    (cos2 x 0 1 0 1 1.)
    )
  )

(cosx 0.)