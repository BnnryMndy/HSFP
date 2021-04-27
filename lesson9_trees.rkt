#lang scheme

(display "tree lesson")
(newline)

;(list 4 2 6 1 3 7 5) -> (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))

(define listToTree
  (λ (inputList)
    (insertListToTree inputList null)
    )
  )

(define insertElementTree
  (λ (element tree)
    (if (null? tree) 
        (list element null null)
        (if (< element (car tree))
            (list (car tree) (insertElementTree element (cadr tree)) (caddr tree))
            (if (> element (car tree))
                (list (car tree) (cadr tree) (insertElementTree element (caddr tree)))
                tree
                )
            )
        )
    )
  )

(define insertListToTree
  (λ (inputList tree)
    (if (null? inputList)
        tree
        (insertListToTree (cdr inputList) (insertElementTree (car inputList) tree))
        )
    )
  )

(listToTree (list 4 2 6 1 3 7 5))


; TO DO: дописать функцию
(define TreeToSortedList
  (λ (tree)
    (if (null? (cadr tree))
        ()
        ())
    )
  )

(define rightRotateTree
  (λ (element tree)
    (if (null? (caddr tree))
        (list (car tree) (cadr tree) element)
        (list (car tree)
              (cadr tree)
              (rightRotateTree element (caddr tree))
              )
        )
    )
  )