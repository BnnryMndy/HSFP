#lang scheme

(define sdf1
  (λ (x k sum n)
    (if(= k n)
       (+ sum (/(- 1 (expt x n))
                (+ 1 (expt x (* 2 n)))))
       (sdf1 x k (+ sum (/(- 1 (expt x n))
                (+ 1 (expt x (* 2 n))))) (+ n 1))
       )
    )
  )

(define sdf
  (λ (x k)
    (sdf1 x k 0 0)
    )
  )

(sdf 2 1)

(define ch
  (λ (g lst sum)
     (if (null? (cdr (cdr (cdr lst))))
         sum
         (ch g (cdr lst) (+ sum (g (car lst) (car (cdr (cdr (cdr lst)))))))
         )
    )
  )


(define checkthis
  (λ (g lst)
    (ch g lst 0)
    )
  )

(checkthis max (list 1 2 3 4 5 6 7))
