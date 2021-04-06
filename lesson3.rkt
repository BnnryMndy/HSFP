#lang scheme
(display "Lesson 3")
(newline)

(define (d54)
  (define (d54a eps x xs)
    (if(> eps (abs (- x xs)))
       x
       (d54a eps (/ 5 (+ 4 x)) x)
    )
  )
  
  (d54a 1e-12 0 1)
)

(display "Ex. 1")
(newline)
(d54)
(newline)



(define (pr)

  (define (pr1 eps res 2^-n)
    (if (> eps 2^-n)
       res
       (pr1 eps (* res (+ 1 2^-n)) (* 2^-n 1/2))
    )
   )
  (pr1 1e-12 1 1/2)
)

(display "Ex. 2")
(newline)
(pr)
(newline)



(define (a^n a n)
  (if (= n 1)
     a
     (if(even? n)
        (a^n (* a a) (/ n 2))
        (* a (a^n a (- n 1)))
     )
  )
)    

(display "Ex. 3")
(newline)
(a^n 2 6)
(newline)



(define (bisec x1 x2 f)
  (if (< (abs (- x2 x1)) 1e-12)
      x1
      (if (> (* (f x1) (f(/ (+ x1 x2) 2)) 0))
          (bisec (/ (+ x1 x2) 2) x2 f)
          (bisec x1 (/ (+ x1 x2) 2) f)
      )
  )
)

(define (p2 x1 x2 f)
  (if (> (f x1) 0)
      (if (> (f x2) 0)
          "Нет корней"
          (bisec x1 x2 f))
      (if (> (f x2) 0)
          (bisec x1 x2 f)
          "Нет корней"
  )
)
  )

(define (f x) (- (* x x) 6))

(display "Ex. 4")
(newline)
(p2 0 8 f)
(newline)