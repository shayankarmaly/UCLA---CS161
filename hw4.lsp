; Shayan Karmaly
; Professor Guy
; CS 161 Fall '22

; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;

(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
  ; using the variable index definition = (n-1) *k + c.
  (+ (* ( - n 1) k) c) ; inner most expression is (n-1), the enclosing expression would be (n-1) *k, and the outermost expression would be (n-1) *k +c
  )

;(print (node2var (1 2 3)))

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;

(defun at-least-one-color (n c k)
  (cond
    
    ; base case/error checking for if c < 0 or if k < c because we wouldn't have a valid color if the maximum color index is less than our color index
    ;if not the base case, then we find the color c and append that to the beginning of 
    ; a new list of clauses that are > c
    ((> c k)nil)
    (T 
        (append 
          (list(node2var n c k)) 
          (at-least-one-color n (+ c 1) k)        
        )
    )
  )

)

;(print (at-least-one-color 5 3 6))

; basically helps at-most-one-color by starting at the current index until the max index and finds 
; all locations that a color cannot be valid. 

(defun find-invalid-colors (n k e1 e2)
  (cond 
    ; if maximum color index is exceeded
    ((< k e2)nil) 
    (T 
      ; append searching from the start to the end like we mentioned above. 
      (cons 
        (append (list (* (node2var n e1 k) -1))(list (* (node2var n e2 k) -1))) (find-invalid-colors n k e1 (+ e2 1)) ; keep recursively calling til we reach the end. 
      )
    )
  )

)

; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;

(defun at-most-one-color (n c k)
  (cond 
    ; if max index == the color index return nil because this would be invalid
    ((= k c)nil)
    
    ; else we append the combination of all the clauses that satisfy the constraint as well as 
    ; continue to recursively call on the list to keep checking if there's at-most-one-color
    (T
      (append 
        (find-invalid-colors n k c (+ c 1)) 
        (at-most-one-color n (+ c 1) k)
      )
    )
  )
  )

  ;(print (at-most-one-color 5 3 6))

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
;we get exactly one color from the set if we can combine the functions that produce at-least-one-color
;and at-most-one-color if there is one color that satisfies the conditions 

(defun generate-node-clauses (n k)
    (append 
      (list (at-least-one-color n 1 k))  ; combining the set of of functions thta are a union of at-least-one-color & at-most-one-color so we 
      (at-most-one-color n 1 k)          ;just need to append the two functions into a list.          
                                          
    )
  )

;(print (generate-node-clauses 2 4))


(defun generate-edge-clause-checker (c k e1 e2)
  (cond 
    ((< k c)nil) ; if our maximum color index is greater than our color index, then we return 
                  ; nil because this is invalid
    (T (let*
          (
            ; call our function node2var from above on the first and second edge. This will give us a list
            ; that only includes where the two edges DONT share the same color
            (different-colors (list (- (node2var e1 c k)) (-(node2var e2 c k)) )
            )
          )
          ; once we're done finding the list of cases where the edges aren't the same color, (^), 
          ; append this list to the rest of the list 
            (cons different-colors (generate-edge-clause-checker (+ c 1) k e1 e2))
          
        )
    )
 
  
  )

)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;

(defun generate-edge-clauses (e k)
  ; will pass in the first and second edge into our helper function that 
  ; checks for us where both ends of edge e don't have the same color

  (generate-edge-clause-checker 1 k (first e) (second e))
)


;(print (generate-edge-clauses '(2 4) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun
