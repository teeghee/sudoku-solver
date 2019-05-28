;; Sudoku solver
;;
;; Made by Taekyung Kim.


;; Synopsis
;;
;; A simple sudoku solver written in chez scheme.

(define-record-type cell
  (fields row col room entry)
  (protocol
   (lambda (new)
     (lambda (row col entry)
       (new row col (+ (* 3 (div row 3))
		       (div col 3))
	    entry))))
  (nongenerative))

;; A board consists of 9x9 = 81 cells in it, arranged as follows:
;;
;;    | 0 1 2 | 3 4 5 | 6 7 8 | 
;; ---+-------+-------+-------+-
;;  0 | room  | room  | room  |
;;  1 |  #0   |  #1   |  #2   |
;;  2 |       |       |       |
;; ---+-------+-------+-------+-
;;  3 | room  | room  | room  |
;;  4 |  #3   |  #4   |  #5   |
;;  5 |       |       |       |
;; ---+-------+-------+-------+-
;;  6 | room  | room  | room  |
;;  7 |  #6   |  #7   |  #8   |
;;  8 |       |       |       |
;; ---+-------+-------+-------+-
;;
;; A cell may have already assigned number from 1 to 9, or it may be
;; an undecided cell.  The whole point to solve Sudoku is to find
;; appropriate numbers that fit in these undecided cells.

;; candidates -- given undecided cell, output the list of candidates
;; of possible numbers that can fit into the cell, determined under
;; the given decided list

(define candidates
  (case-lambda
    [(a-cell decided-list)
     (candidates a-cell decided-list (map add1 (iota 9)))]
    [(a-cell decided-list candid-list)
     (cond [(null? decided-list)
	    (list (length candid-list) candid-list)]
	   [(= (cell-row a-cell)
	       (cell-row (car decided-list)))
	    (candidates a-cell
			(cdr decided-list)
			(remove (cell-entry (car decided-list))
				candid-list))]
	   [(= (cell-col a-cell)
	       (cell-col (car decided-list)))
	    (candidates a-cell
			(cdr decided-list)
			(remove (cell-entry (car decided-list))
				candid-list))]
	   [(= (cell-room a-cell)
	       (cell-room (car decided-list)))
	    (candidates a-cell
			(cdr decided-list)
			(remove (cell-entry (car decided-list))
				candid-list))]
	   [else
	    (candidates a-cell
			(cdr decided-list)
			candid-list)])]))

(define guess-tree
  (case-lambda
    [(unknown-list decided-list)
     (guess-tree unknown-list decided-list '())]
    [(unknown-list decided-list tree)
     (if (null? unknown-list)
	 tree
	 (let* ([an-unknown-cell (car unknown-list)]
		[tree-elem
		 (cons an-unknown-cell
		       (candidates
			an-unknown-cell decided-list))])
	   (guess-tree (cdr unknown-list)
		       decided-list
		       (put-into-sorted
			tree-elem tree
			(lambda (x y)
			  (< (cadr x) (cadr y)))))))]))

(define move-elements
  (lambda (fromlist tolist)
    (if (null? fromlist)
	tolist
	(move-elements (cdr fromlist)
		       (cons (car fromlist) tolist)))))

(define put-into-sorted
  (case-lambda
    [(elem lst proc)
     (if (null? lst)
	 (cons elem lst)
	 (put-into-sorted elem lst proc '()))]
    [(elem lst proc pre-consing)
     (cond [(null? lst)
	    (move-elements pre-consing (cons elem '()))]
	   [(proc elem (car lst))
	    (move-elements pre-consing (cons elem lst))]
	   [else
	    (put-into-sorted
	     elem (cdr lst) proc (cons (car lst) pre-consing))])]))





;; hardest part --- solver

(define chosen-number
  (lambda (a-cell)
    (car (cell-entry a-cell))))

;; remove-all --- removes all the elements of <base-list> which are
;; also elements of <remove-list>, i.e. the intersection of
;; <base-list> and <remove-list> is removed from <base-list>
(define remove-all
  (lambda (base-list remove-list)
    (if (null? remove-list)
	base-list
	(remove-all (remove (car remove-list) base-list)
		    (cdr remove-list)))))

(define forward
  (lambda (tree record)
    (if (null? tree)
	record
	(let* ([the-cell (caar tree)]
	       [the-row (cell-row the-cell)]
	       [the-col (cell-col the-cell)]
	       [the-room (cell-room the-cell)]
	       [first-possibilities (caddar tree)]
	       [filtered (filter (lambda (x)
				   (or (= the-row (cell-row x))
				       (= the-col (cell-col x))
				       (= the-room (cell-room x))))
				 record)]
	       [candidates (remove-all first-possibilities
				       (map cell-entry filtered))])
	  (if (null? candidates)
	      #f
	      (exists
	       values
	       (map (lambda (can)
		      (forward (cdr tree)
			       (cons 
				(make-cell the-row the-col can)
				record)))
		    candidates)))))))


(define make-final-solution-vector
  (lambda (soln decided)
    (let ([combined (append decided soln)]
	  [answer (make-vector 81)])
      (for-each
       (lambda (a-cell)
	 (vector-set! answer
		      (+ (* 9 (cell-row a-cell))
			 (cell-col a-cell))
		      (cell-entry a-cell)))
       combined)
      answer)))

(define print-out-to-string
  (lambda (soln-vec)
    (let ([stringified (make-vector 81)])
      (do ((i 0 (add1 i)))
	  ((= i 81)
	   (apply string-append (vector->list stringified)))
	(let ([num (vector-ref soln-vec i)])
	  (if (= (mod i 9) 8)
	      (vector-set!
	       stringified i
	       (string-append (number->string num) "\n"))
	      (vector-set!
	       stringified i
	       (string-append (number->string num) " "))))))))


;; Getting problem data

(define input-problem-file-name
  "~/sudoku/problem.txt")

(define initial-board
  (call-with-input-file input-problem-file-name
    (lambda (inp)
      (let arrange ([i 0]
		    [decided-cell-list '()]
		    [unknown-cell-list '()]
		    [element (get-datum inp)])
	(cond [(= i 81)
	       (list decided-cell-list
		     unknown-cell-list)]
	      [(and (number? element)
		    (>= element 1)
		    (<= element 9))
	       (arrange (add1 i)
			(cons (make-cell (div i 9)
					 (mod i 9) element)
			      decided-cell-list)
			unknown-cell-list
			(get-datum inp))]
	      [else
	       (arrange (add1 i)
			decided-cell-list
			(cons (make-cell (div i 9)
					 (mod i 9) 'undecided)
			      unknown-cell-list)
			(get-datum inp))])))))

(define initial-decided
  (car initial-board))

(define initial-unknown
  (cadr initial-board))

(define initial-tree
  (guess-tree initial-unknown initial-decided))

(define a-solution
  (forward initial-tree '()))

(define a-solution-vector
  (make-final-solution-vector a-solution initial-decided))

(display (print-out-to-string a-solution-vector))
