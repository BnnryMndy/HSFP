#lang scheme

(define a (list (list 1 2 3) 1 2))

(car a)

(cdr a)

(cons 1 a)

(display a)
(newline)

(define b (list 1 2 4 5))

(car b)

(cdr b)

(cons (cdr b) b)


(define len
  (lambda (lst)
  (define len1 (lambda (lst n)
    (if (null? lst)
        n
        (len1 (cdr lst) (+ n 1))
    )
   )
   )
  (len1 lst 0)
 )
)

(len (list 1 2 3))


(define nel
  (lambda (lst n)
    (if (null? lst)
     null
     (if (= n 1)
         (car lst)
         (nel (cdr lst) (- n 1))
     )
     )
  )
)


(nel (list 3 2 1) 4)


(define dnel
  (lambda (lst n)
    (if (null? lst)
       null
       (if (= n 1)
           (cdr lst)
           (cons (car lst) (dnel (cdr lst) (- n 1))))
    )
  )
)
(dnel (list 1 2 3 4 5) 3)



(define appnd
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (appnd (cdr list1) list2)))
    )
  )


(appnd (list 1 2 3) (list 4 5 6))


(define revers
  (lambda (lst)
    (define rev1
      (lambda (list1 list2)
              (if (null? list1)
                  list2
                  (rev1 (cdr list1) (cons (car list1) list2)))
              )
      )
    (rev1 lst null)
    )
 )


(revers (list 1 2 3 (list 4 5 6) 7 8 9 0))


(define revers+
  (lambda (lst)
    (define rev1
      (lambda (list1 list2)
              (if (null? list1)
                  list2
                  (rev1 (cdr list1) (cons (if (list? (car list1)) (revers+ (car list1)) (car list1)) list2)))
              )
      )
    (rev1 lst null)
    )
 )

(revers+ (list 1 2 3 (list 4 5 6) 7 8 9 0))

(display "lesson 6")
(newline)

(define maxlist
  (lambda (lst)
    (define maxlist2
      (lambda (lst t)
        (if(null? lst)
           t
           (if (list? (car lst))
               (maxlist2 (cdr lst) (maxlist2 (car lst) t))
               (if (>(car lst) t)
                   (maxlist2 (cdr lst) (car lst))
                   (maxlist2 (cdr lst) t)
                   )
           )
           )
        )
      )
    (if (null? lst)
        null
        (maxlist2 (cdr lst) (car lst))
        )
    )
  )


(maxlist (list 1 2 (list 6 7) 4 5))




(define findlst
  (lambda (lst x)
    (if (null? lst)
        false
        (if(list? (car lst))
           (if (findlst (car lst) x)
               true
               (findlst (cdr lst) x)
               )
           (if(=(car lst) x)
              true
              (findlst (cdr lst) x)
           )
        )
      )
  )
)

(findlst (list 1 2 3 (list 8 9 (list 7)) 4 (list 8 9 (list 95) 5) 6) 5)