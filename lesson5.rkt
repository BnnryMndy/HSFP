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