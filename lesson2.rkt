#lang scheme
(display "__factorio__")
(newline)

(define (fact a)
  (if(= a 0)
     1
     (* a (fact (- a 1)))
  )
)

(fact 4)


(define (factr n)
  (factorio n 1)
)

(define (factorio n n!)
  (if(= n 0)
     n!
     (factorio (- n 1) (* n n!))
   )
)

(factr 4)

(newline)
(display "__Summ_of_squares__")
(newline)

(define (summ_of_squares n)
 (summs n 0) 
)

(define (summs n summ)
  (if (= n 0)
      summ
      (summs (- n 1) (+ (* n n) summ))
   )
)


(define (s+ n)
  (* n (+ n 1) (+ n n 1) 1/6)
)

(summ_of_squares 100)
(s+ 100)

(newline)
(display "__T-T__")
(newline)

(define (test_x x)
  (/ 1 (* (- 1 x) (- 1 x)))
)

(test_x 0.6)

(define (sum_x x)
  (if (< (abs x) 1)
      (sum_x_s x 1 0 1e-12 1)
      "Раскроется")
  
)

(define (sum_x_s x n summ eps x^nm1)
  (if(> eps (abs (* n x^nm1)))
     summ
     (sum_x_s x (+ n 1) (+ summ (* n x^nm1)) eps (* x^nm1 x))
  )
)

(sum_x 0.6)


(newline)
(display " __{7|7|7}__")
(newline)
(display "| [jackpot] |")
(newline)
(display "|           |")
(newline)
(display "| [0000000] |")
(newline)
(display "| [       ] |")
(newline)
(display "| [00000 0] |")
(newline)
(display "| [0000 00] |")
(newline)
(display "| [000 000] |")
(newline)
(display "| [00 0000] |")
(newline)
(display "| [0 00000] |")
(newline)
(display "|___________|")
(newline)


(define (s7)
  (s7a 0 1e-12 1)
)


(define (s7a x eps xprev)
  (if(< (abs (- x xprev)) eps)
     x
     (s7a (sqrt (- 7 (sqrt (+ 7 x)))) eps x)
))

(s7)  