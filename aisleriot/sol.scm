; AisleRiot - sol.scm
; Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
;
; This game is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
; USA

;; Constants:

(define jack 11)
(define queen 12)
(define king 13)
(define ace 1)
(define joker 0)

(define club 0)
(define diamond 1)
(define heart 2)
(define spade 3)

(define black 0)
(define red 1)

(define down 0)
(define right 1)

;; Global variables:

(define DECK '())

;; NEW-GAME PROCEDURES
; these may be used in game files during the new-game procedure.

; This procedure MUST be called at the start of the new-game procedure.
(define (initialize-playing-area)
  (reset-surface)
  (set! FLIP-COUNTER 0)
  (set! SLOTS 0)
  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (get-vert-start))
  (set! IN-GAME #f)
  (set! MOVE '())
  (set-statusbar-message "")
  (set! HISTORY '()))

; create a 52 card deck (puts list of cards into DECK)
(define (make-standard-deck)
  (if (= ace 14)
      (set! DECK (make-standard-deck-list-ace-high 2 club))
      (set! DECK (make-standard-deck-list-ace-low ace club))))

; create a 54 card deck with 2 jokers.
(define (make-joker-deck)
  (if (= ace 14)
     (set! DECK (cons (make-card joker club) (cons (make-card joker diamond) 
		 (make-standard-deck-list-ace-high 2 club))))
     (set! DECK (cons (make-card joker club) (cons (make-card joker diamond) 
		 (make-standard-deck-list-ace-low ace club))))))
     
; create a double deck of 104 cards (puts list of cards into DECK)
(define (make-standard-double-deck)
  (if (= ace 14)
      (set! DECK (append (make-standard-deck-list-ace-high 2 club) (make-standard-deck-list-ace-high 2 club)))
      (set! DECK (append (make-standard-deck-list-ace-low ace club) (make-standard-deck-list-ace-low ace club)))))

 ; makes a deck from init-value to kings
(define (make-deck-list-ace-low init-value value suit)
   (if (eq? king value)
      (if (eq? spade suit)
	  (list (make-card king spade))
	  (cons (make-card value suit) 
		(make-deck-list-ace-low 
		 init-value init-value (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-deck-list-ace-low init-value (+ 1 value) suit))))
 
 ; makes a deck from init-value to aces
(define (make-deck-list-ace-high init-value value suit)
   (if (eq? 14 value)
      (if (eq? spade suit)
	  (list (make-card ace spade))
	  (cons (make-card value suit) 
		(make-deck-list-ace-high 
		 init-value init-value (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-deck-list-ace-high init-value (+ 1 value) suit))))
 


; shuffle the card list in DECK
(define (shuffle-deck)
  (let* ((vec (list->vector DECK))
	 (len (vector-length vec)))
    (set! DECK (shuffle-deck-helper vec 0 len))))
					     
; The playing area is divided into slots, where cards can be placed.
; Each slot can hold any amount of cards.  The slots are identified 
; using numbers assigned in order of their creation. The deck of cards
; held in DECK should be assigned to one of the slots on creation.
; (You may then create another deck and place it in a later slot).
;
; The slots are added to the board from left to right until the
; add-carriage-return-slot procedure is called, which starts a new line.
; A space may be added using the add-blank-slot procedure. These false
; slots are not assigned identifiers.

(define (add-blank-slot)
  (get-and-increment-position))

(define (add-carriage-return-slot)
  (linefeed-position))

; The real slots come in three varieties:
; A slot in which only the topmost card is visible:
(define (add-normal-slot cards)
  (add-slot (set-tag! (new-slot cards 
				(list 'normal (get-and-increment-position))))))

; A slot in which all the cards are visible, arranged as an overlapped pile:
; (either proceeding to the right or down).
(define (add-extended-slot cards direction)
  (if (= right direction)
      (add-slot (set-tag! (new-slot cards 
				    (list 'expanded-right 
					  (get-and-increment-position)))))
      (add-slot (set-tag! (new-slot cards 
				    (list 'expanded 
					  (get-and-increment-position)))))))

; A slot in only the n topmost cards are visible:
(define (add-partially-extended-slot cards direction n)
  (if (= right direction)
      (add-slot (set-tag! (new-slot cards 
				    (list 'partially-expanded-right 
					  (get-and-increment-position) n))))
      (add-slot (set-tag! (new-slot cards 
				    (list 'partially-expanded 
					  (get-and-increment-position) n))))))

; Cards may be dealt off one slot (usually the one containing the deck)
; and onto a list of other slots using these procedures:
(define (deal-cards target-slot-id slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (remove-card target-slot-id))
	(deal-cards target-slot-id (cdr slot-list)))))

(define (deal-cards-face-up target-slot-id slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (make-visible (remove-card target-slot-id)))
	(deal-cards-face-up target-slot-id (cdr slot-list)))))

;; GENERAL GAME PROCEDURES
; these may be used in game files at any time.

;; Procedures that change slot contents:

; turn the top card of a slot over (face up to face down and vice versa)
(define (flip-top-card slot-id)
  (add-card! slot-id (flip-card (remove-card slot-id))))

; turn the top card of a slot face side up 
(define (make-visible-top-card slot-id)
  (add-card! slot-id (make-visible (remove-card slot-id))))

; add a card onto the top of a slot
(define (add-card! slot-id card)
  (set-cards! slot-id (cons card (get-cards slot-id))))

; add a list of cards onto the top of a slot
(define (add-cards! slot-id cards)
  (set-cards! slot-id (append cards (get-cards slot-id))))

; remove (and return) the top card from a slot
(define (remove-card slot-id)
  (let ((cards (get-cards slot-id)))
    (set-cards! slot-id (cdr cards))
    (car cards)))

;; Utilities

; deal a card from the stock-slot to the waste-slot.
; when the stock slot is empty than the waste slot will be flipped back
; onto the stock unless the flip limit has been reached.
; an optional forth argument indicates the number of cards to deal.
(define (flip-stock stock-slot waste-slot flip-limit . rest)
  (if (empty-slot? stock-slot)
      (and (< FLIP-COUNTER flip-limit)
	   (set! FLIP-COUNTER (+ 1 FLIP-COUNTER))
	   (flip-deck stock-slot waste-slot))
      (or (let loop ((i (if (null? rest) 1 (car rest))))
	    (and (> i 0)
		 (not (empty-slot? stock-slot))
		 (add-card! waste-slot (flip-card (remove-card stock-slot)))
		 (loop (- i 1))))
	  #t)))

; turn the cards in the waste slot over and add them to the stock-slot.
(define (flip-deck stock-slot waste-slot)
  (and (not (empty-slot? waste-slot))
       (add-card! stock-slot (flip-card (remove-card waste-slot)))
       (or (flip-deck stock-slot waste-slot)
	   #t)))

;; Procedures for manipulating cards:

; NB: In order to use these procedures you must remove the cards 
;     from their slots and then replace them after applying the procedure 
;     (as in the make-top-card-visible procedure above)
(define (flip-card card)
  (list (car card) (cadr card) (not (caddr card))))

(define (make-visible card)
  (list (car card) (cadr card) #t))

;; Procedures that provide information only:

; card procedures
(define (is-visible? card)
  (caddr card))

(define (get-suit card) 
  (cadr card))

(define (get-color card)
  (cond ((eq? (get-suit card) club) black)
	((eq? (get-suit card) spade) black)
	((eq? (get-suit card) heart) red)
	((eq? (get-suit card) diamond) red)
	(#t "Unknown color")))

(define (get-value card) 
  (car card))

; slot procedures
(define (get-cards slot-id)
  (cadr (get-slot slot-id)))

(define (empty-slot? slot-id)
  (null? (get-cards slot-id)))

(define (get-top-card slot-id)
  (let ((cards (get-cards slot-id)))
    (if (null? cards)
	'()
	(car cards))))

;; Utilities - need more of these:
(define (cards-eq? card1 card2)
  (and (eq? (get-value card1) (get-value card2))
       (eq? (get-suit card1) (get-suit card2))))

(define (is-red? card)
  (eq? red (get-color card)))

(define (is-black? card)
  (eq? black (get-color card)))

(define (is-joker? card)
  (= (get-value card) joker))

(define (set-ace-low)  (set! ace 1))

(define (set-ace-high) (set! ace 14))

; use to compare two cards when aces are treated as high:
(define (ace-high-order value)
  (remainder (+ 11 value) 13))

(define (check-same-suit-list card-list)
  (or (< (length card-list) 2)
      (and (= (get-suit (car card-list)) (get-suit (cadr card-list)))
	   (check-same-suit-list (cdr card-list)))))

(define (check-same-color-list card-list)
  (or (< (length card-list) 2)
      (and (eq? (is-red? (car card-list)) (is-red? (cadr card-list)))
	   (check-same-color-list (cdr card-list)))))

(define (check-alternating-color-list card-list)
  (or (< (length card-list) 2)
      (and (eq? (is-black? (car card-list)) (is-red? (cadr card-list)))
	   (check-alternating-color-list (cdr card-list)))))

(define (check-straight-descending-list card-list)
  (or (< (length card-list) 2)
      (and (= (get-value (car card-list)) (- (get-value (cadr card-list)) 1))
	   (check-straight-descending-list (cdr card-list)))))

; debugging aid:
(define (display-list . objs)
  (map display objs) (newline))

; hint procedures: these will be superceeded shortly.
(define (get-value-name value)
  (cond ((eq? value ace) "ace")
	((eq? value 2) "two")
	((eq? value 3) "three")
	((eq? value 4) "four")
	((eq? value 5) "five")
	((eq? value 6) "six")
	((eq? value 7) "seven")
	((eq? value 8) "eight")
	((eq? value 9) "nine")
	((eq? value 10) "ten")
	((eq? value jack) "jack")
	((eq? value queen) "queen")
	((eq? value king) "king")
	(#t "Unknown value")))

(define (get-suit-name suit)
  (cond ((eq? suit club) "clubs")
	((eq? suit spade) "spades")
	((eq? suit heart) "hearts")
	((eq? suit diamond) "diamonds")
	(#t "Unknown suit")))

(define (get-joker-name card) 
  (if (is-black? card) "black joker" "red joker"))

(define (get-name card)
  (if (is-joker? card)
      (get-joker-name card)
      (string-append (get-value-name (get-value card)) 
		     " of "
		     (get-suit-name (get-suit card)))))

(define (move-n-cards! start-slot end-slot cards)
  (add-cards! end-slot cards))

(define (remove-n-cards slot-id n)
  (set-cards! slot-id (nthcdr n (get-cards slot-id))))

(define (deal-cards-from-deck deck slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (car deck))
	(deal-cards-from-deck (cdr deck) (cdr slot-list)))))

(define (deal-cards-face-up-from-deck deck slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (make-visible (car deck)))
	(deal-cards-face-up-from-deck (cdr deck) (cdr slot-list)))))


(define (set-cards! slot-id new_cards)
;  (if IN-GAME 
;      (record-move slot-id new_cards (get-cards slot-id)))
  (set-cards-c! slot-id new_cards))

(define (make-card value suit)
  (list value suit #f))

(define (make-standard-deck-list-ace-high value suit)
  (if (eq? ace value)
      (if (eq? spade suit)
	  (list (make-card ace spade))
	  (cons (make-card value suit) 
		(make-standard-deck-list-ace-high 2 (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-standard-deck-list-ace-high (+ 1 value) suit))))

(define (make-standard-deck-list-ace-low value suit)
  (if (eq? king value)
      (if (eq? spade suit)
	  (list (make-card king spade))
	  (cons (make-card value suit) 
		(make-standard-deck-list-ace-low 1 (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-standard-deck-list-ace-low (+ 1 value) suit))))

(define (shuffle-deck-helper deck ref1 len)
  (if (zero? len)
      '()
      (let* ((ref2 (+ ref1 (random len)))
	     (val-at-ref2 (vector-ref deck ref2)))
	(vector-set! deck ref2 (vector-ref deck ref1))
	(cons val-at-ref2 (shuffle-deck-helper deck (+ ref1 1) (- len 1))))))

(define (new-slot deck placement)
  (list #f deck placement))

(define (set-tag! slot)
  (set! SLOTS (+ 1 SLOTS))
  (cons (- SLOTS 1) (cdr slot)))

(define (get-and-increment-position)
  (let ((retval (list HORIZPOS VERTPOS)))
    (set! HORIZPOS (+ HORIZPOS (get-horiz-offset)))
    retval))

(define (linefeed-position)
  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (+ VERTPOS (get-vert-offset))))

(define (register-undo-function function data)
  (set! MOVE (cons '(function data) (cdr MOVE))))

; common lisp procedure not provided in guile 1.3
(define (nthcdr n lst)
  (if (zero? n) lst (nthcdr (+ -1 n) (cdr lst))))

;; INTERNAL procedures

; global variables
(define FLIP-COUNTER 0)
(define SLOTS 0)
(define HORIZPOS 0)
(define VERTPOS 0)
(define MOVE '())
(define HISTORY '())
(define FUTURE '())
(define IN-GAME #f)

; called from C:
(define (start-game)
  (set! IN-GAME #t))

; called from C:
(define (end-move)
  (if (not (= 0 (length MOVE)))
      (begin
	(set! HISTORY (cons MOVE HISTORY))
	(set! FUTURE '())
	(set! MOVE '()))))

(define (return-cards card-positions slot-id)
  (and (not (= 0 (length card-positions)))
       (set-cards! slot-id (car card-positions))
       (return-cards (cdr card-positions) (+ 1 slot-id))))

(define (give-status-message)
  #t)

(define (eval-move move)
  (return-cards (caddr move) 0)
  ((car move) (cadr move))
  (give-status-message))

; called from C:
(define (undo)
  (and (not (null? HISTORY))
       (record-move -1 '())
       (eval-move (car HISTORY))
       (set! FUTURE (cons MOVE FUTURE))
       (set! HISTORY (cdr HISTORY))
       (set! MOVE '())))

; called from C:
(define (redo)
  (and (not (null? FUTURE))
       (record-move -1 '())
       (eval-move (car FUTURE))
       (set! HISTORY (cons MOVE HISTORY))
       (set! FUTURE (cdr FUTURE))
       (set! MOVE '())))

(define (undo-func data)
  (set-score! (car data))
  (set! FLIP-COUNTER (cadr data)))
;(register-undo-function undo-func '(score FLIP-COUNTER))
	     
(define (snapshot-board slot-id moving-slot old-cards)
  (cond ((>= slot-id SLOTS)
	 '())
	((= slot-id moving-slot)
	 (cons old-cards 
	       (snapshot-board (+ 1 slot-id) moving-slot old-cards)))
	(#t
	 (cons (get-cards slot-id) 
	       (snapshot-board (+ 1 slot-id) moving-slot old-cards)))))

; called from C:
(define (record-move slot-id old-cards)
  (set! MOVE (list undo-func 
		   (list (get-score) FLIP-COUNTER)
		   (snapshot-board 0 slot-id old-cards))))

; called from C:
(define (discard-move)
  (set! MOVE '()))
