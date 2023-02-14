; Shayan Karmaly
; UID: 805554581
; Professor Guy
; CS 161 Fall '22

;; This function takes a single integer argument 'N' and returns the 'Nth' Padovan number.

( defun SEQ (N)

; if we hit our base case where seq(0) = seq(1) = seq(2) = 1
     (if (or (= N 0) (or (= N 1) (= N 2)))

     ; only found either seq(0), seq(1), seq(2)
         1 

         ; else we recursively call seq(n) = seq(n-1) + seq(n-2) + seq(n-3
             (+ (SEQ (- N 1)) (SEQ(- N 2)) (SEQ(- N 3)))
             ))

;; This function takes a single numeric argument 'N' and returns the number of additions 
;; our SEQ function requires to compute the 'Nth' Padovan number.

( defun SUMS (N)

    ; if we hit our base case where sums(0) = sums(1) = sums(2) = 0
    (cond ((or ( = N 2) (= N 1) (= N 0))0) ; return 0 since this is our base case 

        ; else we recursively call sums(n) = sums(n-2) + sums(n-3)
         (T(+(SUMS(- N 2)) (SUMS(- N 3))1) )
    ))   

;; This function takes an argument TREE that represents a tree and returns an anonymized tree 
;; with the same structure, but all values, symbols, and numbers are replaced by a 0. 

( defun ANON (TREE)

    ; check if its an empty list
    ( cond ((null TREE) '()) 

    ; if ther is only a single leaf node in TREE, then just return that single atom in TREE
      ((atom TREE) '0)


    ;else we recursively call on the rest of the TREE
    ;and take the first element of the rest of the TREE passed into ANON as well as the 
    ;rest of the List

      (T(cons ( ANON(car TREE)) (ANON(cdr TREE))))
  ))