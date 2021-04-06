#lang scheme


(display "Matrix sum")
(newline)
(newline)


(define matrixLineSum
      (lambda (firstMatrixLine SecondMatrixLine)
        (if(null? firstMatrixLine)
           null
           (cons  (+
                     (car firstMatrixLine)
                     (car SecondMatrixLine)
                     )
                  (matrixLineSum
                     (cdr firstMatrixLine)
                     (cdr SecondMatrixLine)
                     )
                  )
           )
        )
      )

(define matrixSum
  (lambda (a b)
    (if (null? a)
        null
        (cons (matrixLineSum (car a) (car b))
              (matrixSum (cdr a) (cdr b)
                   )
              )
        )
    )
)


(matrixSum
 (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
 (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
)

(newline)
(display "Matrix composition")
(newline)

(define numberOnMatrixComposition
  (lambda (Number Matrix)
    (if (null? (car Matrix))
        null
        (cons (a+cb Number (listcar Matrix))
              (numberOnMatrixComposition Number (listcdr Matrix))
              )
        )
    )
  )

(define a+cb
  (lambda (a b)
    (if (null? b)
        0
        (+ (* (car a) (car b))
           (a+cb (cdr a) (cdr b))
           )
        )
    )
  )

(define listcar
  (lambda (Matrix)
         (if (null? Matrix)
             null
             (cons (car (car Matrix))
                   (listcar (cdr Matrix))
                   )
             )
         )
  )

(define listcdr
  (lambda (Matrix)
         (if (null? Matrix)
             null
             (cons (cdr (car Matrix))
                   (listcdr (cdr Matrix))
                   )
             )
         )
  )

(define matrixComposition
  (lambda (FirstMatrix SecondMatrix)
    (if (null? FirstMatrix)
        null
        (cons (numberOnMatrixComposition
                   (car FirstMatrix)
                   SecondMatrix
                  )
              (matrixComposition
                  (cdr FirstMatrix)
                   SecondMatrix)
              )
        )
    )
  )


(matrixComposition
 (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
 (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
 )


(newline)
(display "transposition Matrix")
(newline)

(define transposition
  (lambda (Matrix)
    (if (null? (car Matrix))
        null
        (cons (listcar Matrix)
              (transposition (listcdr Matrix))
              )
        )
    )
  )


(transposition (list (list 1 2 3) (list 1 2 3) (list 1 2 3)))