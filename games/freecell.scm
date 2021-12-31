;;; freecell.scm -- Free Cell game for AisleRiot.

;; Copyright (C) 1998, 2003 Changwoo Ryu

;; Author: Changwoo Ryu <cwryu@adam.kaist.ac.kr>

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (aisleriot interface) (aisleriot api) (ice-9 format))

;;; Commentary:

;; FREECELL
;;
;; * The 4 slots in the left-top are called "freecells". (F? in the below)
;; * The 4 slots in the right-top are called "homecells". (H? in the below)
;; * The 8 slots in the bottom are called "fields". (D? in the below)
;;
;;  -------------------------------------------
;;  |                                         |
;;  |(0)  (1)  (2)  (3)    (4)  (5)  (6)  (7) |
;;  | F1   F2   F3   F4     H1   H2   H3   H4 |
;;  |                                         |
;;  |                                         |
;;  | (8)  (9)  (10) (11) (12) (13) (14) (15) |
;;  |  D1   D2   D3   D4   D5   D6   D7   D8  |
;;  |                                         |
;;  -------------------------------------------

;;; Code:

;;
;; Constants
;;
(define freecell-1 0)
(define freecell-2 1)
(define freecell-3 2)
(define freecell-4 3)
(define homecell-1 4)
(define homecell-2 5)
(define homecell-3 6)
(define homecell-4 7)
(define field-1    8)
(define field-2    9)
(define field-3    10)
(define field-4    11)
(define field-5    12)
(define field-6    13)
(define field-7    14)
(define field-8    15)

(define freecells (list freecell-1 freecell-2 freecell-3 freecell-4))
(define homecells (list homecell-1 homecell-2 homecell-3 homecell-4))
(define fields (list field-1 field-2 field-3 field-4
                     field-5 field-6 field-7 field-8))
(define half-fields (list field-1 field-2 field-3 field-4))

;;
;; Initial cards
;;
(define (deal-initial-setup)
  (let ((fields (list field-1 field-2 field-3 field-4
			       field-5 field-6 field-7 field-8))
	(half-fields (list field-1 field-2 field-3 field-4)))
    (deal-cards-face-up-from-deck DECK
				  (append fields fields fields
					  fields fields fields
					  half-fields))))

;;
;; Utilities
;;

(define (freecell? slot)
  (and (>= slot freecell-1) (<= slot freecell-4)))

(define (homecell? slot)
  (and (>= slot homecell-1) (<= slot homecell-4)))

(define (field? slot)
  (and (>= slot field-1) (<= slot field-8)))

(define (slot-type slot)
  (cond ((freecell? slot) 'freecell)
	((homecell? slot) 'homecell)
	((field? slot) 'field)))

(define (opposite-color color)
  (if (eq? color red) black red))
      

;;
;; Utilities for the homecells
;;

;; homecell id which holds the suit or an empty slot if there is no slot.
(define (homecell-by-suit suit)
  (define (p? slot)
    (and (not (empty-slot? slot))
	 (= (get-suit (get-top-card slot)) suit)))
  (cond ((p? homecell-1) homecell-1)
	((p? homecell-2) homecell-2)
	((p? homecell-3) homecell-3)
	((p? homecell-4) homecell-4)
	(#t (any-empty-homecell))))

;; An empty homecell's id, if any
(define (any-empty-homecell)
  (cond ((empty-slot? homecell-1) homecell-1)
	((empty-slot? homecell-2) homecell-2)
	((empty-slot? homecell-3) homecell-3)
	((empty-slot? homecell-4) homecell-4)
	(else #f)))

(define (homecell-join? prev next)
  (and (eq? (get-suit prev) (get-suit next))
       (eq? (+ (get-value prev) 1) (get-value next))))

(define (get-color-homecells color)
  (define (iter n l)
    (if (< n homecell-1)
	l
	(if (eq? (get-top-card n) color)
	    (iter (- n 1) (cons n l))
	    (iter (- n 1) l))))
  (iter homecell-4 '()))

;;
;; Utilities for freecells
;;

;; The total number of empty freecells
(define (empty-freecell-number)
  (do ((i freecell-1 (+ i 1))
       (sum 0 (+ sum (if (empty-slot? i) 1 0))))
      ((> i freecell-4) sum)))

;; An empty freecell's id, if any
(define (any-empty-freecell)
  (cond ((empty-slot? freecell-1) freecell-1)
	((empty-slot? freecell-2) freecell-2)
	((empty-slot? freecell-3) freecell-3)
	((empty-slot? freecell-4) freecell-4)
	(else #f)))

;;
;; Utilities for fields
;;

(define (field-join? lower upper)
  (and (not (eq? (get-color lower) (get-color upper)))
       (eq? (+ (get-value lower) 1) (get-value upper))))

(define (field-sequence? card-list)
  (or (null? card-list)
      (null? (cdr card-list))
      (and (field-join? (car card-list) (cadr card-list))
	   (field-sequence? (cdr card-list)))))

(define (empty-field-number)
  (do ((i field-1 (+ i 1))
       (sum 0 (+ sum (if (empty-slot? i) 1 0))))
      ((> i field-8) sum)))

;;
;; How to move cards
;;

(define (movable-to-homecell? card-list homecell-id)
  (and (= (length card-list) 1)
       (if (empty-slot? homecell-id)
           (eq? (get-value (car card-list)) ace)
           (homecell-join? (car (get-cards homecell-id)) (car card-list)))))

(define (move-to-homecell card-list homecell-id)
	(and
		(= (length card-list) 1)
		(move-card-to-homecell (car card-list) homecell-id)
	)
)

(define (move-card-to-homecell card homecell-id)
	(cond
		; if the homecell is empty, we can add an ace to it.
		((and
			(empty-slot? homecell-id)
			(eq? (get-value card) ace)
			(add-to-score! 1)
			(add-card! homecell-id card)
			(update-auto (get-suit card) (get-value card)))
		#t)
		; Put a +1 card into the homecell, whose suit is same.
		((and
			(not (empty-slot? homecell-id))
			(homecell-join? (car (get-cards homecell-id)) card)
			(add-to-score! 1)
			(add-card! homecell-id card)
			(update-auto (get-suit card) (get-value card)))
		#t)
		(#t #f)
	)
)

;; Version of move-to-field that only tests a move or supermove.
(define (movable-to-field? start-slot card-list field-id)
  (and (field-sequence? card-list)
       (<= (length card-list)
           (* (+ (empty-freecell-number) 1)
              (expt 2 (max (- (empty-field-number)
                               (if (empty-slot? field-id) 1 0)
                               (if (empty-slot? start-slot) 1 0))
                            0))))
       (or (empty-slot? field-id)
           (let ((dest-top (car (get-cards field-id))))
             (and (field-sequence? (append card-list (list dest-top))))))))


(define (move-to-field start-slot card-list field-id)
  (and (movable-to-field? start-slot card-list field-id)
       (add-cards! field-id card-list)))

(define (movable-to-freecell? card-list freecell-id)
  (and (= (length card-list) 1)
       (empty-slot? freecell-id)))

(define (move-to-freecell card-list freecell-id)
	(and
		(= (length card-list) 1)
		(move-card-to-freecell (car card-list) freecell-id)
	)
)

(define (move-card-to-freecell card freecell-id)
	(and
		(not (boolean? freecell-id))
		(empty-slot? freecell-id)
		(add-card! freecell-id card)
	)
)

;;
;; Auto move stuffs
;;

(def-save-var highest-club 0)
(def-save-var highest-diamond 0)
(def-save-var highest-heart 0)
(def-save-var highest-spade 0)

(define (update-auto suit value)
	(cond
		((eq? suit club) (set! highest-club value))
		((eq? suit diamond) (set! highest-diamond value))
		((eq? suit heart) (set! highest-heart value))
		((eq? suit spade) (set! highest-spade value))
	)
)

(define (max-auto-red)
	(min
		(+ 2 (min highest-club highest-spade))
		(+ 3 (min highest-diamond highest-heart))
	)
)

(define (max-auto-black)
	(min
		(+ 2 (min highest-diamond highest-heart))
		(+ 3 (min highest-club highest-spade))
	)
)

(define (move-low-cards slot)
  (or
   (and
    (not (homecell? slot))
    (not (empty-slot? slot))
    (let ((card (get-top-card slot)))
      (if (= (get-color card) red)
	  (and
	   (<= (get-value card) (max-auto-red))
	   (move-card-to-homecell card (homecell-by-suit (get-suit card)))
	   (remove-card slot)
	   (delayed-call ((lambda (x) (lambda () (move-low-cards x))) 0))
	   )
	  (and
	   (<= (get-value card) (max-auto-black))
	   (move-card-to-homecell card (homecell-by-suit (get-suit card)))
	   (remove-card slot)
	   (delayed-call ((lambda (x) (lambda () (move-low-cards x))) 0))
					;	(move-low-cards 0)
	   )
	  )
      )
    )
   (if (< slot field-8)
       (move-low-cards (+ 1 slot))
       #t
       )
   )
  )

;;
;; Callbacks & Initialize the game
;;

;; Set up a new game.
(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)
  
  ;; set up the board

  ; freecells
  (add-normal-slot '() 'reserve)			; 0
  (set! HORIZPOS (- HORIZPOS (/ 1 24)))
  (add-normal-slot '() 'reserve)			; 1
  (set! HORIZPOS (- HORIZPOS (/ 1 24)))
  (add-normal-slot '() 'reserve)			; 2
  (set! HORIZPOS (- HORIZPOS (/ 1 24)))
  (add-normal-slot '() 'reserve)			; 3
  (set! HORIZPOS (+ HORIZPOS 0.25))

  ; homecells
  (add-normal-slot '() 'foundation)			; 4
  (set! HORIZPOS (- HORIZPOS (/ 1 24)))
  (add-normal-slot '() 'foundation)			; 5
  (set! HORIZPOS (- HORIZPOS (/ 1 24)))
  (add-normal-slot '() 'foundation)			; 6
  (set! HORIZPOS (- HORIZPOS (/ 1 24)))
  (add-normal-slot '() 'foundation)			; 7
  (add-carriage-return-slot)

  ; fields
  (add-extended-slot '() down 'tableau)		; 8
  (add-extended-slot '() down 'tableau)		; 9
  (add-extended-slot '() down 'tableau)		; 10
  (add-extended-slot '() down 'tableau)		; 11
  (add-extended-slot '() down 'tableau)		; 12
  (add-extended-slot '() down 'tableau)		; 13
  (add-extended-slot '() down 'tableau)		; 14
  (add-extended-slot '() down 'tableau)		; 15

  (add-blank-slot)
  (deal-initial-setup)
  (update-auto club 0)
  (update-auto diamond 0)
  (update-auto heart 0)
  (update-auto spade 0)

  (set! board-hash (make-hash-table hash-size))
  

  (list 8 3.5)
)

(define (button-pressed slot card-list)
  (cond ((homecell?   slot) #f)
	((field?      slot) (field-sequence? card-list))
	((freecell?   slot) #t)))

(define (droppable? start-slot card-list end-slot)
        (and (not (= start-slot end-slot))
             (cond
               ((homecell? end-slot) (movable-to-homecell? card-list end-slot))
               ((field?    end-slot) (movable-to-field? start-slot card-list end-slot))
	       ((freecell? end-slot) (movable-to-freecell? card-list end-slot))
               (else #f))))

(define (button-released start-slot card-list end-slot)
	(and
		(not (= start-slot end-slot))
		(cond
			((homecell? end-slot) (move-to-homecell card-list end-slot))
			((field?    end-slot) (move-to-field    start-slot card-list end-slot))
			((freecell? end-slot) (move-to-freecell card-list end-slot))
		)
		(move-low-cards 0)
	)
)
  
(define (button-clicked slot)
  ; (FIXME)
  #f)

(define (button-double-clicked slot)
	(and
		(not (empty-slot? slot))
		(let ((card (get-top-card slot)))
			(and
			        (move-card-to-freecell card (any-empty-freecell))
			        (remove-card slot)
			        (move-low-cards 0)
			)
		)
	)
)

;; Condition for fail -- no more cards to move
(define (game-over)
  ; (FIXME)
  (not (game-won)))

;; Condition for win -- all the cards in homecells
(define (game-won)
  (and (= 13 (length (get-cards homecell-1)))
       (= 13 (length (get-cards homecell-2)))
       (= 13 (length (get-cards homecell-3)))
       (= 13 (length (get-cards homecell-4)))))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  ; (FIXME)
  #f)

;------------------------------------------------------------------------------
; Additions for hint feature
;
; Written by Matthew V. Ball <mball@siliconashes.net>
;
; The rest of this file is devoted to implementing an intelligent hint
; feature.  The general search algorithm creates a tree, with each unique
; board position representing a node.  These nodes are stored in a hash
; table so that the search does not repeat the work for a particular
; board position.  Furthermore, the move function sorts the cards within
; a given board so that different card orders are still treated as the 
; same board.
;
; Each searched board is given a qualitative value based first
; on "Mobility", then "Weight", then "Depth".  Here is a brief definition of
; these terms:
;
; Mobility - The maximum number of cards possible to move from one tableau
;   to another.  This equals (1 + (# of freecells)) * 2^(# of open tableaus))
; Weight - The number of cards in play that are not part of a sequence.
;   For example, placing a 5 on a 6 reduces the board weight by 1, unless the
;   5 was already on a different 6.
; Depth - The number of moves between the current node and the best node.
;
; In particular, the algorithm maximizes Mobility up until mobility-thresh,
; after which point additional mobility is not considered.  Both Weight and
; Depth are minimized.  By minimizing Depth, the algorithm will tend to
; optimize for the shortest path, eliminating unnecessary moves.  This
; becomes especially important when determining which of two winning moves
; to make (there are generally two winning moves: the last move made, if
; it is reversible, and the winning move that approaches the final solution).
;
; This algorithm will eventually find a solution, or determine that a
; solution is not possible.  However, in the interest of not searching for
; too long, the search algorithm will stop searching after a specified number
; of nodes, then return the best move found so far.  If the user presses
; help multiple times, then the search algorithm starts again where it left
; off and finds a better move.  If the search algorithm ever does find a
; solution, it will remember the entire solution in the hash table so that
; the hint feature can immediately return the next move when asked to do so.
;
; Here are definitions for some generic data structures used in this
; algorithm:
;
; Board vector - The board positions are stored in vectors (for no particular
;   reason -- I wanted to experiment with different data types).
;
; index	description
; ----- -----------
; 0-3	Freecell cards - Card list containing card, or '() if empty
; 4-7	Homecells - integer with highest card on homecell for each suit.
; 8-15	Field cards - Card list containing cards on each tableau.
;
; Board Attributes - This is a vector containg some working information
;   about an associated board.
;
; index description
; ----- -----------
; 0	Board mobility (size of largest group that can move to a field) (scalar)
; 1	Board weight (Number of groups in fields and freecells) (scalar)
; 2	Board outcome (win = 1, lose = 0, unknown = #f)
; 3	Depth to best board outcome
; 4	Inuse: Is this board currently being looked at? (#t or #f)
; 5	Best known value of sub-tree
; 6	List of possible moves, sorted from best to worst (#f if not generated)
;	Move definition: ((next-board . next-attributes) 
;                         start-slot card card-count end-slot)
;
; The hash table stores associated pairs of the board vector and board
; attributes.  This is often described as (board . info) in the following text.

;;-----------------------------------------------------------------------------
;; Constants

; Set debug to #t for verbose output 
(define debug #f)

; These constants refer to indices within a board attributes vector
(define index-mobility 0)
(define index-weight   1)
(define index-outcome  2)
(define index-depth    3)
(define index-inuse    4)
(define index-value    5)
(define index-moves    6)

; These constants are the possible values for a board outcome
(define outcome-win    1)
(define outcome-lose   0)
(define outcome-unknown #f)

; This is the highest mobility for which the algorithm strives.
; Any mobility larger than the threshhold is disregarded.
; 6 represents an open tableau and two cards in the reserve slots
; (generally, if the algorithm can create an open tableau, the game
; can be solved)
(define mobility-thresh 6)

; These constants indicate which board vector indices represent the state
; of the homecells.
(define board-foundation 4)
(define board-club    (+ board-foundation club))
(define board-diamond (+ board-foundation diamond))
(define board-heart   (+ board-foundation heart))
(define board-spade   (+ board-foundation spade))

; These constants affect the hash table and search algorithm
(define hash-size (- (expt 2 17) 1)) ; A Mersenne prime (2^17 - 1) ~128k
(define board-node-max  50) ; number of board positions to visit each time.
(define traverse-node-max 1000) ; prevents stack overflow

; These constants define values used in constructing the board value.
(define weight-factor   100) ; effect of weight on final score
(define mobility-factor (* 100 weight-factor)) ; effect of mobility
(define max-move-value  (* 1280 mobility-factor)) ; solution found
(define min-move-value  (- 0 max-move-value))     ; no solution found

; value-bias is the amount to bias the previously best move value when
; searching sub-trees.  A more negative number tends to favor a depth-first
; search instead of a breadth-first search.
(define value-bias      -50)

;;-----------------------------------------------------------------------------
;; Global variables

; This is a hash table that holds information about the board
; positions analyzed by the search function.
(define board-hash #()) ; This variable is initialized in new-game
(define visited-nodes 0) ; Number of board positions created for this search.
(define traversed-nodes 0) ; Number of board positions traversed through


;;-----------------------------------------------------------------------------
;; Functions

; Returns the best move found by the search algorithm
(define (get-hint)
  (if debug (display "get-hint\n"))
  (set! visited-nodes 0)
  (set! traversed-nodes 0)
  (let* ((board (copy-master-board))
	 (info  (get-board-info board)))
    (analyze-board board info 0)
    (let* ((moves (vector-ref info index-moves)))
      (if debug
	(begin
	  (display "visited nodes: ") (display visited-nodes) (newline)
	  (display "traversed nodes: ") (display traversed-nodes) (newline)
	  (display (list-head (vector->list info) 6))
	  (newline)
	  (display-moves board moves)
	  (newline)
	  (display-best-move-trace board moves)))
      (create-help-list board moves))))

; Displays the sequence of best moves found so far by the search. (Debug only)
; Note that the best sequence is occasionally not available depending on
; how the hint function terminates the search.  In these cases, this function
; displays "Non-decreasing" and shows the available moves at the point
; it got confused.
; move format: ((board . info) start-slot card card-count end-slot)
(define (display-best-move-trace board moves)
  (if (not (or (null? moves)
	       (eq? moves #f)))
    (let* ((best-move (car moves))
	   (next-moves (vector-ref (cdar best-move) index-moves)))
      (display-moves board (list best-move))
      (if (not (or (null? next-moves) (eq? next-moves #f)))
	(if (> (vector-ref (cdar best-move) index-depth)
	       (vector-ref (cdaar next-moves) index-depth))
	  (display-best-move-trace (caar best-move) next-moves)
	  (begin
	    (display "Non Decreasing:\n")
	    (display-moves board moves)
	    (display "Trace of best-move:\n")
	    (display-moves (caar best-move) next-moves)))))))

; Displays a list of moves, relative to a given board position (debug only)
; move format: ((board . info) start-slot card card-count end-slot)
(define (display-moves board moves)
  (if (not (null? moves))
    (begin
      (display (list-head (vector->list (cdaar moves)) 6))
      (display (create-help-list board moves))
      (newline)
      (display-moves board (cdr moves)))))

; Creates the move description returned by get-hint.
; move format: ((board . info) start-slot card card-count end-slot)
(define (create-help-list board moves)
  (if (null? moves)
    (list 0 (G_"No moves are possible. Undo or start again."))
    (let* ((best-move (car moves))
	   (from-card (caddr best-move))
	   (to-slot   (list-ref best-move 4))
	   (to-stack  (vector-ref board to-slot)))
      (if (eq? (vector-ref (cdar best-move) index-outcome) outcome-lose)
	(list 0 (G_"The game has no solution. Undo or start again."))
	(hint-move (find-card-slot from-card) (find-card (find-card-slot from-card) from-card)
	      (cond ((freecell? to-slot) (find-empty-slot freecells))
		    ((homecell? to-slot)
		     (if (equal? 0 to-stack)
		         (find-empty-slot homecells)
		         (find-card-slot (list to-stack (get-suit from-card) #t))))
		    ((null? to-stack) (find-empty-slot fields))
		    (else (find-card-slot (car to-stack)))))))))

; Returns a vector copy of the master board for use as the initial
; node in the search.
(define (copy-master-board)
  (let ((freecell-cards (map get-cards freecells))
	(homecell-cards (list highest-club
			      highest-diamond
			      highest-heart
			      highest-spade))
	(field-cards    (map get-cards fields)))
    (list->vector (append
		    (sort freecell-cards compare-cards)
		    homecell-cards
		    (sort field-cards compare-cards)))))

; Recursively analyzes board positions.  This function is the heart of
; the search algorithm.  It will continue to search sub-nodes as long as
; each newly searched board has a value that is greater than prev-best.
; Otherwise, this function saves the value of the best board position found
; in this sub-tree, and returns to the caller
;
; Parameters:
;   board - vector containing board position to analyze
;   info - vector describing board (board attributes)
;   prev-best - best board value seen in nodes above this node.
(define (analyze-board board info prev-best)
  ; increment the number of traversed nodes so that we can estimate the
  ; stack depth and ensure it doesn't get too deep.
  (set! traversed-nodes (+ 1 traversed-nodes))
  
  ; Check wether we have already generated moves for this board position.
  ; If not generate the moves now.
  (if (eq? (vector-ref info index-moves) #f)
    (vector-set! info index-moves (get-board-moves board)))
  (vector-set! info index-inuse (+ 1 (vector-ref info index-inuse)))

  ; set this node to outcome-lose so that we don't revisit the same node.
  ; This also becomes the default value if we return early
  (vector-set! info index-value min-move-value)
  (vector-set! info index-outcome outcome-lose)

  ; Sort the moves from best to worst based on value
  (let ((moves (sort (vector-ref info index-moves) move-compare)))
    (vector-set! info index-moves moves)

    ; Check whether there are any moves that don't lose.  (If not, exit
    ; with loss)
    (if (and (not (null? moves))
	     (not (eq? (vector-ref (cdaar moves) index-outcome) outcome-lose)))

      ; Determine whether to traverse deeper, or to go back up the tree
      (if (and (eq? (vector-ref (cdaar moves) index-outcome) #f)
	       (< visited-nodes board-node-max)
	       (< traversed-nodes traverse-node-max)
	       (>= (vector-ref (cdaar moves) index-value) prev-best))
	(begin
	  ; Traverse into the best available move
	  (analyze-board 
	    (caaar moves)
	    (cdaar moves)
	    (if (null? (cdr moves))
	      prev-best
	      (max prev-best (+ value-bias
				(vector-ref (cdaadr moves) index-value)))))
	  ; Repeat analysis of this node in case another move beats the
	  ; current best
	  (analyze-board board info prev-best))

	; Copy the best outcome and move to previous node
	(copy-outcome-info! info (cdaar moves)))
      ; else leave outcome set to 'outcome-lose' and go up to previous node
      ))
  (vector-set! info index-inuse (+ -1 (vector-ref info index-inuse))))

; copies the inportant board information from source to dest
(define (copy-outcome-info! dest source)
  (vector-set! dest index-outcome     (vector-ref source index-outcome))
  (vector-set! dest index-value (+ -1 (vector-ref source index-value)))
  (vector-set! dest index-depth (+  1 (vector-ref source index-depth))))

; Sort compare function -- compares two moves (see also get-move-value)
; Rules:
;  if a position is a winner, move it to the front.
;  else if a position is a loser, move it to the back.
;  else if the mobility of both positions is above a threshold, then
;       compare positions only using board weight
;  else compare using mobility first, then use board weight for a tie,
;       then use depth as a further tie-breaker.
;
; returns #t if left move is better than right move
; returns #f if both positions are equal or right move is better
; input format: ((board . info) start-slot card card-count end-slot)
(define (move-compare left right)
  (> (vector-ref (cdar left)  index-value)
     (vector-ref (cdar right) index-value)))

; Returns a list of possible board moves
(define (get-board-moves board)
  (get-board-moves-from-slots 
    board (append fields freecells)))

; Returns a list of board moves from a given list of slots
(define (get-board-moves-from-slots board slots)
  (if (null? slots)
    '()
    (append (get-board-moves-from-cards 
	      board 
	      (car slots)
	      1
	      (vector-ref board (car slots)))
	    (get-board-moves-from-slots board (cdr slots)))))

; Returns a list of board moves from a given slot with a given height of cards
(define (get-board-moves-from-cards board slot height cards)
  (if (null? cards) 
    '()
    (append (if (and (not (null? (cdr cards)))
		     (field-join? (car cards) (cadr cards)))
	      (get-board-moves-from-cards 
		board 
		slot 
		(+ height 1) 
		(cdr cards))
	      '() )
	    (get-moves-from-card-to-slots
	      board 
	      slot 
	      height 
	      (car cards)
	      (append 
		(remove-redundant-open-slots board fields)
		(get-leftmost-open-freecell board)
		homecells)))))

; returns a list containing the slot number for the left-most open freecell,
; or '() if none are open
(define (get-leftmost-open-freecell board)
  (cond ((null? (vector-ref board freecell-1)) (list freecell-1))
	((null? (vector-ref board freecell-2)) (list freecell-2))
	((null? (vector-ref board freecell-3)) (list freecell-3))
	((null? (vector-ref board freecell-4)) (list freecell-4))
	(else '())))

; Returns a list of field slot numbers with redundant open slots removed
(define (remove-redundant-open-slots board slots)
  (if (null? slots)
    '()
    (if (null? (vector-ref board (car slots)))
      (cons (car slots) (remove-all-open-fields board (cdr slots)))
      (cons (car slots) (remove-redundant-open-slots board (cdr slots))))))

; Returns a list of fields slot number with all open slots removed
(define (remove-all-open-fields board slots)
  (if (null? slots)
    '()
    (if (null? (vector-ref board (car slots)))
      (remove-all-open-fields board (cdr slots))
      (cons (car slots) (remove-all-open-fields board (cdr slots))))))

; determines the possible moves from a given card (at a particular source-slot
; and with a given height) to a set of destination slots.
(define (get-moves-from-card-to-slots board source-slot height card slots)
  (if (null? slots)
    '()
    (append 
      (let* ((dest-slot (car slots))
	     (dest-cards (vector-ref board dest-slot)))
	(if (or (and (homecell? dest-slot)
		     (= height 1)
		     (= (get-suit card) (- dest-slot homecell-1))
		     (= (get-value card) (+ dest-cards 1)))
	        (and (freecell? dest-slot)
		     (not (freecell? source-slot))
		     (= height 1)
		     (null? dest-cards))
	        (and (field? dest-slot)
		     (or (and (null? dest-cards)
			      (or (freecell? source-slot)
				  (not
				    (= height
				       (length
					 (vector-ref board source-slot))))))
			 (and (not (null? dest-cards))
			      (field-join? card (car dest-cards))))
		     (or (= height 1)
			 (<= height
			     (get-board-mobility
			       board
			       (if (null? dest-cards) 1 0))))))
	  (let* ((move-cdr (list source-slot card height (car slots)))
		 (move (cons (get-board-info-pair
			       (perform-move board move-cdr))
			     move-cdr)))
	    (if (= (vector-ref (cdar move) index-value) 0)
	      (vector-set! 
	        (cdar move) index-value
	        (quotient 
		  (get-move-value move)
		  (let ((source-cards (list-tail (vector-ref board source-slot)
					         height)))
		    (if (and (not (null? source-cards))
			     (not (freecell? (cadr move)))
			     (field-join?
			       (caddr move)
			       (car source-cards)))
		      2
		      1)))))
	    (list move))
	  '() ))
      (get-moves-from-card-to-slots 
	board 
	source-slot 
	height 
	card 
	(cdr slots)))))

; returns a new board with a given move applied and small cards moved up
;   board - a board vector
;   move - list in the form (source-slot card card-count dest-slot)
;          (This is more precisely a move-cdr)
(define (perform-move board move)
  (set! visited-nodes (+ 1 visited-nodes))
  (let ((new-board (list->vector (vector->list board)))
	(source-stack (vector-ref board (car move)))
	(dest-stack (vector-ref board (cadddr move))))
    (vector-set! new-board (cadddr move)
		 (if (homecell? (cadddr move))
		   (get-value (car source-stack))
		   (append (list-head source-stack (caddr move))
			   dest-stack)))
    (vector-set! new-board (car move) (list-tail source-stack (caddr move)))
    (move-board-low-cards new-board 0)
    (let* ((temp-board (vector->list new-board))
	   (freecell-cards (list-head temp-board 4))
	   (homecell-cards (list-head (list-tail temp-board 4) 4))
	   (field-cards (list-tail temp-board 8)))
      (set! new-board 
	(list->vector (append (sort freecell-cards compare-cards)
			      homecell-cards
			      (sort field-cards compare-cards)))))
    new-board))

; Compares two card stacks and returns #t if the top card from
; card1 is larger than that of card2.
(define (compare-cards card1 card2)
  (> (card-value card1) (card-value card2)))

; Returns 0 if there is no card, or between 1 and 52 for the absolute
; rank of the top card in a stack.  This equates to 4*rank+suit, where
; the suit order is club=0, diamond=1, heart=2, and spade=3.
; format of card: ((rank suit visible) ...) or '()
(define (card-value card)
  (if (null? card)
    0
    (+ (* 4 (caar card)) (cadar card))))

; This function is more or less a copy of move-low-cards, except it
; operates on a local board instead of a global board.
(define (move-board-low-cards board slot)
  (and (not (homecell? slot))
       (not (null? (vector-ref board slot)))
       (let* ((card (car (vector-ref board slot)))
	      (homecell-slot (+ board-foundation (get-suit card)))
	      (homecell-value (vector-ref board homecell-slot)))
	 (if (and (= (get-value card) (+ 1 homecell-value))
		  (or (and (= (get-color card) red)
			   (<= (get-value card) (max-board-auto-red board)))
		      (and (= (get-color card) black)
			   (<= (get-value card) (max-board-auto-black board)))))
	   (begin
	     (vector-set! board (+ board-foundation (get-suit card)) 
			  (get-value card))
	     (vector-set! board slot (cdr (vector-ref board slot)))
	     (move-board-low-cards board 0)))))
  (or (>= slot field-8)
      (move-board-low-cards board (+ 1 slot))))

; Copy of max-auto-red, except uses a local board.
; Returns the maximum rank of the red homecells that is automatically moved.
; This equates to the highest red suit rank that is not useful in play.  In
; other words, it is better to move the lower black suit cards to the
; homecells instead of stacking them on top of a red suit card that is at or
; below this rank.
(define (max-board-auto-red board)
  (min (+ 2 (min (vector-ref board board-club)
		 (vector-ref board board-spade)))
       (+ 3 (min (vector-ref board board-diamond)
		 (vector-ref board board-heart)))))

; see max-board-auto-red and exchange red for black
(define (max-board-auto-black board)
  (min (+ 2 (min (vector-ref board board-diamond) 
		 (vector-ref board board-heart)))
       (+ 3 (min (vector-ref board board-club) 
		 (vector-ref board board-spade)))))

; Returns the value of a move, based on the board information.
; The resulting format generally looks like this: MWWDD, where
;   M is Mobility, WW is 100 - board weight, and DD is 100 - depth.
; input format: ((board . info) start-slot card card-count end-slot)
(define (get-move-value move)
  (let ((info  (cdar move))
	(board (caar move)))
    (let ((mobility (vector-ref info index-mobility))
	  (weight   (vector-ref info index-weight))
	  (outcome  (vector-ref info index-outcome))
	  (inuse    (> (vector-ref info index-inuse) 0))
	  (depth    (vector-ref info index-depth)))
      (cond (inuse                      min-move-value)
	    ((eq? outcome outcome-win)  (- max-move-value depth))
	    ((= weight 0)		(- max-move-value depth))
	    ((eq? outcome outcome-lose) min-move-value)
	    (else (+ (* mobility-factor (min mobility-thresh mobility))
		     (- mobility-factor (* weight-factor weight))
		     (- weight-factor depth)))))))

; generates a board and info pair (board . pair) based on an input board
(define (get-board-info-pair board)
  (cons board (get-board-info board)))

; Returns the information for a particular board position by looking
; in hash table.  If not entry found, creates a new entry in the hash
; table with default information
(define (get-board-info board)
  (or (hash-ref board-hash board)
      (let ((info (vector (get-board-mobility board 0)
			  (get-board-weight board)
			  outcome-unknown ; Outcome not known
			  1     ; each new board has a depth of 1
			  0     ; board is not yet being looked at
			  0     ; position has no value yet
			  #f))) ; no moves generated yet
	; Add new board to hash table
	(hash-set! board-hash board info)
	(if (= (vector-ref info index-weight) 0)
	  (vector-set! info index-outcome outcome-win))
	info)))

; Determines a board's 'Weight' by determining the number of groups within
; the tableaus and the freecells (reserves).  A group is defined as a set
; of consecutive cards that alternate color.
(define (get-board-weight board)
  (define (get-slot-list-weight slots)
    (if (null? slots)
      0
      (+ (get-card-list-weight (vector-ref board (car slots)))
	 (get-slot-list-weight (cdr slots)))))
  (get-slot-list-weight (append freecells fields)))
	    
; returns the 'weight' of a card list, which is the number of distinct runs
(define (get-card-list-weight card-list)
  (cond ((null? card-list)       0)
        ((null? (cdr card-list)) 1)
        (else (+ (get-card-list-weight (cdr card-list))
		 (if (field-join? (car card-list) (cadr card-list)) 0 1)))))

; Returns the board 'Mobility', which is defined as the largest run of cards
; the user could move to another card.
; Parameters:
;   board: board vector
;   adjust: 0 - Compute mobility when moving a stack to another card
;           1 - Compute mobility when moving a stack to an open tableau
(define (get-board-mobility board adjust)
  (* (+ (get-board-free-count board freecells) 1)
     (expt 2 (- (get-board-free-count board fields) adjust))))

; returns the number of open cells available within a given set of cells
(define (get-board-free-count board cells)
  (if (null? cells)
    0
    (+ (get-board-free-count board (cdr cells))
       (if (null? (vector-ref board (car cells))) 1 0))))

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)

;;; freecell.scm ends here


