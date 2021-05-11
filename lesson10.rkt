#lang scheme

(require plot)
(define sum0
  (λ (x1 . xn)
    (define s1
      (λ (listx sum)
        (if (null? listx)
            (/ sum)
            (s1 (cdr listx) (+ sum (/ 1 (car listx))))
            )
        )
      )
    (s1 xn (/ x1))
     )
  )

(sum0 1)
(sum0 1 2)
(sum0 1 2 3)
(sum0 1 2 3 4)

(define deleteDuplicateNumber
  (λ (lst)
    (if (null? lst)
        null
    (if (est? (car lst) (cdr lst))
        (deleteDuplicateNumber (cdr lst))
        (cons (car lst) (deleteDuplicateNumber (cdr lst)))))
  ))

(define est?
  (λ (el lst)
    (if (null? lst)
        #f
        (if (= el (car lst))
            #t
            (est? el (cdr lst))
            )
        )
    )
  )

(deleteDuplicateNumber (list 1 2 3 2 2 4))


(define vl
  (λ (lst)
    (vl1 lst 0)
    )
  )

(define vl1
  (λ (lst gl)
    (if (null? lst)
        gl
        (if (not (list? (car lst)))
            (vl1 (cdr lst) gl)
            (max (vl1 (car lst) (+ 1 gl))
                 (vl1 (cdr lst) gl)
                 )
            )
        )
    )
  )


(vl (list 1 (list (list )) 3 (list (list (list (list )))) 4))


(define koch
  (lambda (lst n)
    (if (> n 0)
        (koch (koch1 lst) (- n 1))
        lst)
    )
  )


(define p3
  (lambda (a b)
    (list (+ (* 1/3 (car a))
             (* 2/3 (car b))
             )
          (+ (* 1/3 (cadr a))
             (* 2/3 (cadr b))
             )
          )
    )
  )

(define p1
  (lambda (a b)
    (list (+ (* 2/3 (car a))
             (* 1/3 (car b))
             )
          (+ (* 2/3 (cadr a))
             (* 1/3 (cadr b))
             )
          )
    )
  )


(define s6(/(sqrt 3) 6))

(define p2
  (λ (a b)
    (list (+ (* 1/2 (car a))
             (* 1/2 (car b))
             (* s6 (cadr a))
             (* -1 s6 (cadr b))
             )
          (+ (* -1 s6 (car a))
             (*  s6 (car b))
             (* 1/2 (cadr a))
             (* 1/2 (cadr b))
             )
          )
    )
  )

(define koch1
  (λ (lst)
    (if (null? (cdr lst))
        lst
    (cons (car lst)
          (cons (p1 (car lst) (cadr lst))
          (cons (p2 (car lst) (cadr lst))
          (cons (p3 (car lst) (cadr lst))
          (koch1 (cdr lst))))))
    ))
  )

(plot (lines (koch (list (list 0. 0.) (list 1. 0.)) 5)))