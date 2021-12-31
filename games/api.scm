; AisleRiot API
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
;
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

(define-module (aisleriot api))

(use-modules (aisleriot interface) (ice-9 format) (ice-9 i18n))

;; This is the encoding of strings returned from e.g. 'format',
;; so we need to set this to "UTF-8" since that's what the C side
;; of aisleriot expects.  Otherwise as per docs, guile sets this
;; from the locale encoding, which would be wrong if the locale is
;; an not UTF-8 locale, and also it seems that even though we call
;; setlocale(3), the guile side does not pick this up, for whatever
;; reason.
;; Bug #733881.
(fluid-set! %default-port-encoding "UTF-8")

;; Define the usual alias for gettext
(define-public (G_ msg) (gettext msg "aisleriot"))

;; Feature masks:
(define-public droppable-feature 1)
(define-public scores-disabled 2)
(define-public dealable-feature 4)

(define-public (set-features . feature-list)
  (set-feature-word! (+ (get-feature-word)
		       (apply + feature-list))))

(define-public jack 11)
(define-public queen 12)
(define-public king 13)
(define-public ace 1)
(define-public joker 0)

(define-public club 0)
(define-public diamond 1)
(define-public heart 2)
(define-public spade 3)

(define-public black 0)
(define-public red 1)

(define-public down 0)
(define-public right 1)

;; Global variables:

(define-public DECK '())

; The list of variables to save when saving the game state
(define-public variable-list '())

;; NEW-GAME PROCEDURES
; these may be used in game files during the new-game procedure.

; This procedure MUST be called at the start of the new-game procedure.
;
; Note that variable-list is not cleared, this is because defines are normally
; done before calling this and we would loose our variable list. At worst
; case we end up saving and restoring variables that are not currently in use 
; (but will be defined) so it will work out OK.
(define-public (initialize-playing-area)
  (reset-surface)
  (set! FLIP-COUNTER 0)
  (set! SLOTS 0)
  (set! HORIZPOS 0)
  (set! VERTPOS 0)
  (set! IN-GAME #f)
  (set! MOVE '())
  (set-statusbar-message " ")
  (set! HISTORY '())
  (set! FOUNDATION-SLOTS '())
  (set! TABLEAU-SLOTS '())
  (set! RESERVE-SLOTS '())
  (set! EDGE-SLOTS '())
  (set! CORNER-SLOTS '())
  (set! TOP-SLOTS '())
  (set! BOTTOM-SLOTS '())
  (set! LEFT-SLOTS '())
  (set! RIGHT-SLOTS '())
  (set-score! 0))

; Use this instead of define for variables which determine the state of
; the game. i.e. anything that isn't a constant. This is so undo/redo
; is transparent. It should behave otherwise identically to define.
(defmacro-public def-save-var (nm value)
  `(begin (define-public ,nm ,value)
          (set! variable-list (cons ',nm variable-list))))

; create a 52 card deck (puts list of cards into DECK)
(define-public (make-standard-deck)
  (if (= ace 14)
      (set! DECK (make-standard-deck-list-ace-high 2 club))
      (set! DECK (make-standard-deck-list-ace-low ace club))))

; create a 54 card deck with 2 jokers.
(define-public (make-joker-deck)
  (if (= ace 14)
     (set! DECK (cons (make-card joker club) (cons (make-card joker diamond) 
		 (make-standard-deck-list-ace-high 2 club))))
     (set! DECK (cons (make-card joker club) (cons (make-card joker diamond) 
		 (make-standard-deck-list-ace-low ace club))))))
     
; create a double deck of 104 cards (puts list of cards into DECK)
(define-public (make-standard-double-deck)
  (if (= ace 14)
      (set! DECK (append (make-standard-deck-list-ace-high 2 club) (make-standard-deck-list-ace-high 2 club)))
      (set! DECK (append (make-standard-deck-list-ace-low ace club) (make-standard-deck-list-ace-low ace club)))))

 ; makes a deck from init-value to kings
(define-public (make-deck-list-ace-low init-value value suit)
   (if (eq? king value)
      (if (eq? spade suit)
	  (list (make-card king spade))
	  (cons (make-card value suit) 
		(make-deck-list-ace-low 
		 init-value init-value (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-deck-list-ace-low init-value (+ 1 value) suit))))
 
 ; makes a deck from init-value to aces
(define-public (make-deck-list-ace-high init-value value suit)
   (if (eq? 14 value)
      (if (eq? spade suit)
	  (list (make-card ace spade))
	  (cons (make-card value suit) 
		(make-deck-list-ace-high 
		 init-value init-value (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-deck-list-ace-high init-value (+ 1 value) suit))))
 
; shuffle the card list
(define (shuffle-card-list card-list)
  (let* ((vec (list->vector card-list))
	 (len (vector-length vec)))
    (shuffle-deck-helper vec '() 0 len)))

; shuffle the card list in DECK
(define-public (shuffle-deck)
  (set! DECK (shuffle-card-list DECK)))

; shuffle the card list in slot
(define-public (shuffle-slot slot-id)
  (set-cards! slot-id (shuffle-card-list (get-cards slot-id))))

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

(define-public (add-blank-slot)
  (get-and-increment-position))

(define-public (add-carriage-return-slot)
  (linefeed-position))

; The real slots come in three varieties:
; A slot in which only the topmost card is visible:
(define-public (add-normal-slot cards . type)
  (add-slot (set-tag! (new-slot cards
				(list 'normal (get-and-increment-position)) type))))

; A slot in which all the cards are visible, arranged as an overlapped pile:
; (either proceeding to the right or down).
(define-public (add-extended-slot cards direction . type)
  (if (= right direction)
      (add-slot (set-tag! (new-slot cards 
				    (list 'expanded-right 
					  (get-and-increment-position)) type)))
      (add-slot (set-tag! (new-slot cards 
				    (list 'expanded 
					  (get-and-increment-position)) type)))))

; A slot in only the n topmost cards are visible:
(define-public (add-partially-extended-slot cards direction n . type)
  (if (= right direction)
      (add-slot (set-tag! (new-slot cards 
				    (list 'partially-expanded-right 
					  (get-and-increment-position) n) type)))
      (add-slot (set-tag! (new-slot cards 
				    (list 'partially-expanded 
					  (get-and-increment-position) n) type)))))

; Cards may be dealt off one slot (usually the one containing the deck)
; and onto a list of other slots using these procedures:
(define-public (deal-cards target-slot-id slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (remove-card target-slot-id))
	(deal-cards target-slot-id (cdr slot-list)))))

(define-public (deal-cards-face-up target-slot-id slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (make-visible (remove-card target-slot-id)))
	(deal-cards-face-up target-slot-id (cdr slot-list)))))

;; GENERAL GAME PROCEDURES
; these may be used in game files at any time.

;; Procedures that change slot contents:

; turn the top card of a slot over (face up to face down and vice versa)
(define-public (flip-top-card slot-id)
  (add-card! slot-id (flip-card (remove-card slot-id))))

; turn the top card of a slot face side up 
(define-public (make-visible-top-card slot-id)
  (add-card! slot-id (make-visible (remove-card slot-id))))

; add a card onto the top of a slot
(define-public (add-card! slot-id card)
  (set-cards! slot-id (cons card (get-cards slot-id))))

; add a list of cards onto the top of a slot
(define-public (add-cards! slot-id cards)
  (set-cards! slot-id (append cards (get-cards slot-id))))

; remove (and return) the top card from a slot
(define-public (remove-card slot-id)
  (let ((cards (get-cards slot-id)))
    (set-cards! slot-id (cdr cards))
    (car cards)))

;; Utilities

(define-public (flippable? stock-slot waste-slot flip-limit)
  (or (not (empty-slot? stock-slot))
      (and (not (empty-slot? waste-slot))
           (or (< flip-limit 0)
               (< FLIP-COUNTER flip-limit)))))

; deal a card from the stock-slot to the waste-slot.
; when the stock slot is empty than the waste slot will be flipped back
; onto the stock unless the flip limit has been reached.
; an optional forth argument indicates the number of cards to deal.
; If the flip limit is negative, it is treated as infinite.
(define-public (flip-stock stock-slot waste-slot flip-limit . rest)
  (if (empty-slot? stock-slot)
      (and (not (empty-slot? waste-slot))
           (or (< flip-limit 0)
	       (< FLIP-COUNTER flip-limit))
	   (set! FLIP-COUNTER (+ 1 FLIP-COUNTER))
	   (flip-deck stock-slot waste-slot))
      (or (let loop ((i (if (null? rest) 1 (car rest))))
	    (and (> i 0)
		 (not (empty-slot? stock-slot))
		 (add-card! waste-slot (flip-card (remove-card stock-slot)))
		 (loop (- i 1))))
	  #t)))

; turn the cards in the waste slot over and add them to the stock-slot.
(define-public (flip-deck stock-slot waste-slot)
  (and (not (empty-slot? waste-slot))
       (add-card! stock-slot (flip-card (remove-card waste-slot)))
       (or (flip-deck stock-slot waste-slot)
	   #t)))

;; Procedures for manipulating cards:

; NB: In order to use these procedures you must remove the cards 
;     from their slots and then replace them after applying the procedure 
;     (as in the make-top-card-visible procedure above)
(define-public (flip-card card)
  (list (car card) (cadr card) (not (caddr card))))

(define-public (make-visible card)
  (list (car card) (cadr card) #t))

;; Procedures that provide information only:

; card procedures
(define-public (is-visible? card)
  (caddr card))

(define-public (get-suit card) 
      (cadr card))

(define-public (get-color card)
  (cond ((eq? (get-suit card) club) black)
	((eq? (get-suit card) spade) black)
	((eq? (get-suit card) heart) red)
	((eq? (get-suit card) diamond) red)
	(#t (G_"Unknown color"))))

(define-public (get-value card)
      (car card))

;; WARNING: This generates a synthetic card that isn't part of the game.
;;          See gaps.scm for an example of its intended use.
(define-public (add-to-value card n)
  (cons (+ (car card) n) (cdr card)))

; slot procedures
(define-public (get-cards slot-id)
  (cadr (get-slot slot-id)))

(define-public (empty-slot? slot-id)
  (null? (get-cards slot-id)))

(define-public (any-slot-empty? slots)
  (if (eq? slots '())
      #f
      (or (empty-slot? (car slots))
          (any-slot-empty? (cdr slots)))))

(define-public (find-empty-slot slots)
  (cond ((null? slots) #f)
        ((empty-slot? (car slots)) (car slots))
        (#t (find-empty-slot (cdr slots)))))

(define-public (find-card-helper card cards n)
  (if (null? cards)
      #f
      (if (equal? (car cards) card)
          n
          (find-card-helper card (cdr cards) (+ n 1)))))

(define-public (find-card slot card)
  (find-card-helper card (get-cards slot) 1))

(define (find-card-slot-helper slot card)
  (if (equal? #f (find-card slot card))
      (find-card-slot-helper (+ 1 slot) card)
      slot))

(define-public (find-card-slot card)
  (find-card-slot-helper 0 card))

; Get the nth card from a slot. Returns #f if n is out of range.
(define-public (get-nth-card slot-id n)
  (let ((cards (get-cards slot-id)))
    (cond ((< n 1) #f)
	  ((> n (length cards)) #f)
	  (#t (list-ref cards (- n 1))))))

(define-public (get-top-card slot-id)
  (let ((cards (get-cards slot-id)))
    (if (null? cards)
	'()
	(car cards))))

;; Utilities - need more of these:
(define-public (suit-eq? card1 card2)
  (eq? (get-suit card1) (get-suit card2)))

(define-public (color-eq? card1 card2)
  (eq? (get-color card1) (get-color card2)))

(define-public (value-eq? card1 card2)
  (eq? (get-value card1) (get-value card2)))

(define-public (cards-eq? card1 card2)
  (and (eq? (get-value card1) (get-value card2))
       (eq? (get-suit card1) (get-suit card2))))

(define-public (is-red? card)
  (eq? red (get-color card)))

(define-public (is-black? card)
  (eq? black (get-color card)))

(define-public (is-joker? card)
  (= (get-value card) joker))

(define-public (set-ace-low)  (set! ace 1))

(define-public (set-ace-high) (set! ace 14))

; use to compare two cards when aces are treated as high:
(define-public (ace-high-order value)
  (remainder (+ 11 value) 13))

(define-public (check-same-suit-list card-list)
  (or (< (length card-list) 2)
      (and (= (get-suit (car card-list)) (get-suit (cadr card-list)))
	   (check-same-suit-list (cdr card-list)))))

(define-public (check-same-color-list card-list)
  (or (< (length card-list) 2)
      (and (eq? (is-red? (car card-list)) (is-red? (cadr card-list)))
	   (check-same-color-list (cdr card-list)))))

(define-public (check-alternating-color-list card-list)
  (or (< (length card-list) 2)
      (and (eq? (is-black? (car card-list)) (is-red? (cadr card-list)))
	   (check-alternating-color-list (cdr card-list)))))

(define-public (check-straight-descending-list card-list)
  (or (< (length card-list) 2)
      (and (= (get-value (car card-list)) (- (get-value (cadr card-list)) 1))
	   (check-straight-descending-list (cdr card-list)))))

; debugging aid:
(define-public (display-list . objs)
  (map display objs) (newline))

; hint procedures
(define-public (get-joker-name card) 
  (if (is-black? card) (G_"the black joker") (G_"the red joker")))

(define (get-name card)
  ; Do not use this function directly. To create a hint for moving a card or
  ; stack of cards, use (hint-move).
  (let ((value (get-value card)) (suit (get-suit card)))
    (if (is-joker? card)
        (get-joker-name card)
        (cond ((eq? suit club) 
               (cond ((eq? value ace) (G_"the ace of clubs"))
                     ((eq? value 2) (G_"the two of clubs"))
                     ((eq? value 3) (G_"the three of clubs"))
                     ((eq? value 4) (G_"the four of clubs"))
                     ((eq? value 5) (G_"the five of clubs"))
                     ((eq? value 6) (G_"the six of clubs"))
                     ((eq? value 7) (G_"the seven of clubs"))
                     ((eq? value 8) (G_"the eight of clubs"))
                     ((eq? value 9) (G_"the nine of clubs"))
                     ((eq? value 10) (G_"the ten of clubs"))
                     ((eq? value jack) (G_"the jack of clubs"))
                     ((eq? value queen) (G_"the queen of clubs"))
                     ((eq? value king) (G_"the king of clubs"))
                     (#t (G_"the unknown card"))))
              ((eq? suit spade) 
               (cond ((eq? value ace) (G_"the ace of spades"))
                     ((eq? value 2) (G_"the two of spades"))
                     ((eq? value 3) (G_"the three of spades"))
                     ((eq? value 4) (G_"the four of spades"))
                     ((eq? value 5) (G_"the five of spades"))
                     ((eq? value 6) (G_"the six of spades"))
                     ((eq? value 7) (G_"the seven of spades"))
                     ((eq? value 8) (G_"the eight of spades"))
                     ((eq? value 9) (G_"the nine of spades"))
                     ((eq? value 10) (G_"the ten of spades"))
                     ((eq? value jack) (G_"the jack of spades"))
                     ((eq? value queen) (G_"the queen of spades"))
                     ((eq? value king) (G_"the king of spades"))
                     (#t (G_"the unknown card"))))
              ((eq? suit heart) 
               (cond ((eq? value ace) (G_"the ace of hearts"))
                     ((eq? value 2) (G_"the two of hearts"))
                     ((eq? value 3) (G_"the three of hearts"))
                     ((eq? value 4) (G_"the four of hearts"))
                     ((eq? value 5) (G_"the five of hearts"))
                     ((eq? value 6) (G_"the six of hearts"))
                     ((eq? value 7) (G_"the seven of hearts"))
                     ((eq? value 8) (G_"the eight of hearts"))
                     ((eq? value 9) (G_"the nine of hearts"))
                     ((eq? value 10) (G_"the ten of hearts"))
                     ((eq? value jack) (G_"the jack of hearts"))
                     ((eq? value queen) (G_"the queen of hearts"))
                     ((eq? value king) (G_"the king of hearts"))
                     (#t (G_"the unknown card"))))
              ((eq? suit diamond) 
               (cond ((eq? value ace) (G_"the ace of diamonds"))
                     ((eq? value 2) (G_"the two of diamonds"))
                     ((eq? value 3) (G_"the three of diamonds"))
                     ((eq? value 4) (G_"the four of diamonds"))
                     ((eq? value 5) (G_"the five of diamonds"))
                     ((eq? value 6) (G_"the six of diamonds"))
                     ((eq? value 7) (G_"the seven of diamonds"))
                     ((eq? value 8) (G_"the eight of diamonds"))
                     ((eq? value 9) (G_"the nine of diamonds"))
                     ((eq? value 10) (G_"the ten of diamonds"))
                     ((eq? value jack) (G_"the jack of diamonds"))
                     ((eq? value queen) (G_"the queen of diamonds"))
                     ((eq? value king) (G_"the king of diamonds"))
                     (#t (G_"the unknown card"))))
              (#t (G_"the unknown card"))))))

(define (hint-get-dest-format to-slot cards)
  (if (null? cards)
      (cond ((member to-slot FOUNDATION-SLOTS) (if (= (length FOUNDATION-SLOTS) 1) (G_"Move ~a onto the foundation.") (G_"Move ~a onto an empty foundation slot.")))
            ((member to-slot TABLEAU-SLOTS) (if (= (length TABLEAU-SLOTS) 1) (G_"Move ~a onto the tableau.") (G_"Move ~a onto an empty tableau slot.")))
            ((member to-slot RESERVE-SLOTS) (if (= (length RESERVE-SLOTS) 1) (G_"Move ~a onto the reserve.") (G_"Move ~a onto an empty reserve slot.")))
            ((member to-slot EDGE-SLOTS) (G_"Move ~a onto an empty edge slot."))
            ((member to-slot CORNER-SLOTS) (G_"Move ~a onto an empty corner slot."))
            ((member to-slot TOP-SLOTS) (G_"Move ~a onto an empty top slot."))
            ((member to-slot BOTTOM-SLOTS) (G_"Move ~a onto an empty bottom slot."))
            ((member to-slot LEFT-SLOTS) (G_"Move ~a onto an empty left slot."))
            ((member to-slot RIGHT-SLOTS) (G_"Move ~a onto an empty right slot."))
            (else (G_"Move ~a onto an empty slot.")))
      (let* ((card (car cards)) (value (get-value card)) (suit (get-suit card)))
             (cond ((is-joker? card)
                    (if (is-black? card) (G_"Move ~a onto the black joker.") (G_"Move ~a onto the red joker.")))
                   ((eq? suit club) 
                    (cond ((eq? value ace) (G_"Move ~a onto the ace of clubs."))
                          ((eq? value 2) (G_"Move ~a onto the two of clubs."))
                          ((eq? value 3) (G_"Move ~a onto the three of clubs."))
                          ((eq? value 4) (G_"Move ~a onto the four of clubs."))
                          ((eq? value 5) (G_"Move ~a onto the five of clubs."))
                          ((eq? value 6) (G_"Move ~a onto the six of clubs."))
                          ((eq? value 7) (G_"Move ~a onto the seven of clubs."))
                          ((eq? value 8) (G_"Move ~a onto the eight of clubs."))
                          ((eq? value 9) (G_"Move ~a onto the nine of clubs."))
                          ((eq? value 10) (G_"Move ~a onto the ten of clubs."))
                          ((eq? value jack) (G_"Move ~a onto the jack of clubs."))
                          ((eq? value queen) (G_"Move ~a onto the queen of clubs."))
                          ((eq? value king) (G_"Move ~a onto the king of clubs."))
                          (#t (G_"Move ~a onto the unknown card."))))
                   ((eq? suit spade) 
                    (cond ((eq? value ace) (G_"Move ~a onto the ace of spades."))
                          ((eq? value 2) (G_"Move ~a onto the two of spades."))
                          ((eq? value 3) (G_"Move ~a onto the three of spades."))
                          ((eq? value 4) (G_"Move ~a onto the four of spades."))
                          ((eq? value 5) (G_"Move ~a onto the five of spades."))
                          ((eq? value 6) (G_"Move ~a onto the six of spades."))
                          ((eq? value 7) (G_"Move ~a onto the seven of spades."))
                          ((eq? value 8) (G_"Move ~a onto the eight of spades."))
                          ((eq? value 9) (G_"Move ~a onto the nine of spades."))
                          ((eq? value 10) (G_"Move ~a onto the ten of spades."))
                          ((eq? value jack) (G_"Move ~a onto the jack of spades."))
                          ((eq? value queen) (G_"Move ~a onto the queen of spades."))
                          ((eq? value king) (G_"Move ~a onto the king of spades."))
                          (#t (G_"Move ~a onto the unknown card."))))
                   ((eq? suit heart) 
                    (cond ((eq? value ace) (G_"Move ~a onto the ace of hearts."))
                          ((eq? value 2) (G_"Move ~a onto the two of hearts."))
                          ((eq? value 3) (G_"Move ~a onto the three of hearts."))
                          ((eq? value 4) (G_"Move ~a onto the four of hearts."))
                          ((eq? value 5) (G_"Move ~a onto the five of hearts."))
                          ((eq? value 6) (G_"Move ~a onto the six of hearts."))
                          ((eq? value 7) (G_"Move ~a onto the seven of hearts."))
                          ((eq? value 8) (G_"Move ~a onto the eight of hearts."))
                          ((eq? value 9) (G_"Move ~a onto the nine of hearts."))
                          ((eq? value 10) (G_"Move ~a onto the ten of hearts."))
                          ((eq? value jack) (G_"Move ~a onto the jack of hearts."))
                          ((eq? value queen) (G_"Move ~a onto the queen of hearts."))
                          ((eq? value king) (G_"Move ~a onto the king of hearts."))
                          (#t (G_"Move ~a onto the unknown card."))))
                   ((eq? suit diamond) 
                    (cond ((eq? value ace) (G_"Move ~a onto the ace of diamonds."))
                          ((eq? value 2) (G_"Move ~a onto the two of diamonds."))
                          ((eq? value 3) (G_"Move ~a onto the three of diamonds."))
                          ((eq? value 4) (G_"Move ~a onto the four of diamonds."))
                          ((eq? value 5) (G_"Move ~a onto the five of diamonds."))
                          ((eq? value 6) (G_"Move ~a onto the six of diamonds."))
                          ((eq? value 7) (G_"Move ~a onto the seven of diamonds."))
                          ((eq? value 8) (G_"Move ~a onto the eight of diamonds."))
                          ((eq? value 9) (G_"Move ~a onto the nine of diamonds."))
                          ((eq? value 10) (G_"Move ~a onto the ten of diamonds."))
                          ((eq? value jack) (G_"Move ~a onto the jack of diamonds."))
                          ((eq? value queen) (G_"Move ~a onto the queen of diamonds."))
                          ((eq? value king) (G_"Move ~a onto the king of diamonds."))
                          (#t (G_"Move ~a onto the unknown card."))))
                   (#t (G_"Move ~a onto the unknown card."))))))

(define-public (hint-move from-slot from-slot-count to-slot)
  (if (= from-slot to-slot)
      (list 0 (format #f (hint-get-dest-format to-slot (list-tail (get-cards to-slot) from-slot-count)) (get-name (get-nth-card from-slot from-slot-count))))
      (list 0 (format #f (hint-get-dest-format to-slot (get-cards to-slot)) (get-name (get-nth-card from-slot from-slot-count))))))

(define-public (hint-click slot-id hint-string)
  (list 0 hint-string))

(define (get-remove-string card)
  (let ((value (get-value card)) (suit (get-suit card)))
       (cond ((is-joker? card)
              (if (is-black? card) (G_"Remove the black joker.") (G_"Remove the red joker.")))
             ((eq? suit club) 
              (cond ((eq? value ace) (G_"Remove the ace of clubs."))
                    ((eq? value 2) (G_"Remove the two of clubs."))
                    ((eq? value 3) (G_"Remove the three of clubs."))
                    ((eq? value 4) (G_"Remove the four of clubs."))
                    ((eq? value 5) (G_"Remove the five of clubs."))
                    ((eq? value 6) (G_"Remove the six of clubs."))
                    ((eq? value 7) (G_"Remove the seven of clubs."))
                    ((eq? value 8) (G_"Remove the eight of clubs."))
                    ((eq? value 9) (G_"Remove the nine of clubs."))
                    ((eq? value 10) (G_"Remove the ten of clubs."))
                    ((eq? value jack) (G_"Remove the jack of clubs."))
                    ((eq? value queen) (G_"Remove the queen of clubs."))
                    ((eq? value king) (G_"Remove the king of clubs."))
                    (#t (G_"Remove the unknown card."))))
             ((eq? suit spade) 
              (cond ((eq? value ace) (G_"Remove the ace of spades."))
                    ((eq? value 2) (G_"Remove the two of spades."))
                    ((eq? value 3) (G_"Remove the three of spades."))
                    ((eq? value 4) (G_"Remove the four of spades."))
                    ((eq? value 5) (G_"Remove the five of spades."))
                    ((eq? value 6) (G_"Remove the six of spades."))
                    ((eq? value 7) (G_"Remove the seven of spades."))
                    ((eq? value 8) (G_"Remove the eight of spades."))
                    ((eq? value 9) (G_"Remove the nine of spades."))
                    ((eq? value 10) (G_"Remove the ten of spades."))
                    ((eq? value jack) (G_"Remove the jack of spades."))
                    ((eq? value queen) (G_"Remove the queen of spades."))
                    ((eq? value king) (G_"Remove the king of spades."))
                    (#t (G_"Remove the unknown card."))))
             ((eq? suit heart) 
              (cond ((eq? value ace) (G_"Remove the ace of hearts."))
                    ((eq? value 2) (G_"Remove the two of hearts."))
                    ((eq? value 3) (G_"Remove the three of hearts."))
                    ((eq? value 4) (G_"Remove the four of hearts."))
                    ((eq? value 5) (G_"Remove the five of hearts."))
                    ((eq? value 6) (G_"Remove the six of hearts."))
                    ((eq? value 7) (G_"Remove the seven of hearts."))
                    ((eq? value 8) (G_"Remove the eight of hearts."))
                    ((eq? value 9) (G_"Remove the nine of hearts."))
                    ((eq? value 10) (G_"Remove the ten of hearts."))
                    ((eq? value jack) (G_"Remove the jack of hearts."))
                    ((eq? value queen) (G_"Remove the queen of hearts."))
                    ((eq? value king) (G_"Remove the king of hearts."))
                    (#t (G_"Remove the unknown card."))))
             ((eq? suit diamond) 
              (cond ((eq? value ace) (G_"Remove the ace of diamonds."))
                    ((eq? value 2) (G_"Remove the two of diamonds."))
                    ((eq? value 3) (G_"Remove the three of diamonds."))
                    ((eq? value 4) (G_"Remove the four of diamonds."))
                    ((eq? value 5) (G_"Remove the five of diamonds."))
                    ((eq? value 6) (G_"Remove the six of diamonds."))
                    ((eq? value 7) (G_"Remove the seven of diamonds."))
                    ((eq? value 8) (G_"Remove the eight of diamonds."))
                    ((eq? value 9) (G_"Remove the nine of diamonds."))
                    ((eq? value 10) (G_"Remove the ten of diamonds."))
                    ((eq? value jack) (G_"Remove the jack of diamonds."))
                    ((eq? value queen) (G_"Remove the queen of diamonds."))
                    ((eq? value king) (G_"Remove the king of diamonds."))
                    (#t (G_"Remove the unknown card."))))
             (#t (G_"Remove the unknown card.")))))

(define-public (hint-remove-top-card slot)
  (hint-click slot (get-remove-string (get-top-card slot))))

(define-public (move-n-cards! start-slot end-slot cards)
  (add-cards! end-slot cards))

(define-public (remove-n-cards slot-id n)
  (set-cards! slot-id (list-tail (get-cards slot-id) n)))

(define-public (deal-cards-from-deck deck slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (car deck))
	(deal-cards-from-deck (cdr deck) (cdr slot-list)))))

(define-public (deal-cards-face-up-from-deck deck slot-list)
  (if (not (null? slot-list))
      (begin 
	(add-card! (car slot-list) (make-visible (car deck)))
	(deal-cards-face-up-from-deck (cdr deck) (cdr slot-list)))))


(define-public (set-cards! slot-id new_cards)
  (set-cards-c! slot-id new_cards))

(define-public (make-card value suit)
  (list value suit #f))

(define-public (make-standard-deck-list-ace-high value suit)
  (if (eq? ace value)
      (if (eq? spade suit)
	  (list (make-card ace spade))
	  (cons (make-card value suit) 
		(make-standard-deck-list-ace-high 2 (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-standard-deck-list-ace-high (+ 1 value) suit))))

(define-public (make-standard-deck-list-ace-low value suit)
  (if (eq? king value)
      (if (eq? spade suit)
	  (list (make-card king spade))
	  (cons (make-card value suit) 
		(make-standard-deck-list-ace-low 1 (+ 1 suit))))
      (cons (make-card value suit) 
	    (make-standard-deck-list-ace-low (+ 1 value) suit))))

(define-public (shuffle-deck-helper deck result ref1 len)
  (if (zero? len)
      result
      (let* ((ref2 (+ ref1 (aisleriot-random len)))
	     (val-at-ref2 (vector-ref deck ref2)))
	(vector-set! deck ref2 (vector-ref deck ref1))
	(shuffle-deck-helper deck (cons val-at-ref2 result) (+ ref1 1) (- len 1)))))

(define-public (new-slot deck placement type)
  (list #f deck placement (if (null? type) 'unknown (car type))))

(define-public (set-tag! slot)
  (case (cadddr slot)
    ((tableau) (set! TABLEAU-SLOTS (cons SLOTS TABLEAU-SLOTS)))
    ((reserve) (set! RESERVE-SLOTS (cons SLOTS RESERVE-SLOTS)))
    ((edge) (set! EDGE-SLOTS (cons SLOTS EDGE-SLOTS)))
    ((corner) (set! CORNER-SLOTS (cons SLOTS CORNER-SLOTS)))
    ((top) (set! TOP-SLOTS (cons SLOTS TOP-SLOTS)))
    ((bottom) (set! BOTTOM-SLOTS (cons SLOTS BOTTOM-SLOTS)))
    ((left) (set! LEFT-SLOTS (cons SLOTS LEFT-SLOTS)))
    ((right) (set! RIGHT-SLOTS (cons SLOTS RIGHT-SLOTS)))
    ((foundation) (set! FOUNDATION-SLOTS (cons SLOTS FOUNDATION-SLOTS))))
  (set! SLOTS (+ 1 SLOTS))
  (cons (- SLOTS 1) (cdr slot)))

(define-public (get-and-increment-position)
  (let ((retval (list HORIZPOS VERTPOS)))
    (set! HORIZPOS (+ HORIZPOS 1))
    retval))

(define-public (linefeed-position)
  (set! HORIZPOS 0)
  (set! VERTPOS (+ VERTPOS 1)))

(define-public (register-undo-function function data)
  (set! MOVE (cons '(function data) (cdr MOVE))))

; set score
(define-public (set-score! value)
  (begin
    (set! SCORE value)
    (update-score (number->locale-string SCORE))
    SCORE))

(define-public (get-score) 
  SCORE)

(define-public (add-to-score! delta)
  (set-score! (+ (get-score) delta)))

(define-public (set-statusbar-message message)
  (set! STATUSBAR-MESSAGE message)
  (set-statusbar-message-c message)
)

;; INTERNAL procedures

; global variables
(define-public FLIP-COUNTER 0)
(define-public SLOTS 0)
(define-public HORIZPOS 0)
(define-public VERTPOS 0)
(define-public MOVE '())
(define-public HISTORY '())
(define-public FUTURE '())
(define-public IN-GAME #f)
(define-public FOUNDATION-SLOTS '())
(define-public TABLEAU-SLOTS '())
(define-public RESERVE-SLOTS '())
(define-public EDGE-SLOTS '())
(define-public CORNER-SLOTS '())
(define-public TOP-SLOTS '())
(define-public BOTTOM-SLOTS '())
(define-public LEFT-SLOTS '())
(define-public RIGHT-SLOTS '())
(define-public SCORE 0)
(define-public STATUSBAR-MESSAGE "")

; called from C:
(define-public (start-game)
  (set! IN-GAME #t))

; called from C:
(define-public (end-move)
  (if (not (= 0 (length MOVE)))
      (begin
	(set! HISTORY (cons MOVE HISTORY))
	(set! FUTURE '())
	(set! MOVE '())
        (if (null? HISTORY)
            (undo-set-sensitive #f)
            (undo-set-sensitive #t))
        (if (null? FUTURE)
            (redo-set-sensitive #f)
            (redo-set-sensitive #t)))))

(define-public (return-cards card-positions slot-id)
  (and (not (= 0 (length card-positions)))
       (set-cards! slot-id (car card-positions))
       (return-cards (cdr card-positions) (+ 1 slot-id))))

(define-public (eval-move move)
  (return-cards (caddr move) 0)
  ((car move) (cadr move)))

; called from C:
(define-public (undo)
  (and (not (null? HISTORY))
       (record-move -1 '())
       (eval-move (car HISTORY))
       (set! FUTURE (cons MOVE FUTURE))
       (set! HISTORY (cdr HISTORY))
       (set! MOVE '())
       (redo-set-sensitive #t)
       (if (null? HISTORY)
           (undo-set-sensitive #f))))

; called from C:
(define-public (redo)
  (and (not (null? FUTURE))
       (record-move -1 '())
       (eval-move (car FUTURE))
       (set! HISTORY (cons MOVE HISTORY))
       (set! FUTURE (cdr FUTURE))
       (set! MOVE '())
       (undo-set-sensitive #t)
       (if (null? FUTURE)
           (redo-set-sensitive #f))))

(define-public (undo-func data)
  (set-score! (car data))
  (set! FLIP-COUNTER (cadr data))
  (set-statusbar-message (caddr data))
  (restore-variables variable-list (cadddr data)))
;(register-undo-function undo-func '(score FLIP-COUNTER))
	     
(define-public (snapshot-board slot-id moving-slot old-cards)
  (cond ((>= slot-id SLOTS)
	 '())
	((= slot-id moving-slot)
	 (cons old-cards 
	       (snapshot-board (+ 1 slot-id) moving-slot old-cards)))
	(#t
	 (cons (get-cards slot-id) 
	       (snapshot-board (+ 1 slot-id) moving-slot old-cards)))))

; called from C:
(define-public (record-move slot-id old-cards)
  (set! MOVE (list undo-func 
                   (list (get-score) FLIP-COUNTER 
                         STATUSBAR-MESSAGE
                         (save-variables variable-list))
                   (snapshot-board 0 slot-id old-cards))))

; called from C:
(define-public (discard-move)
  (set! MOVE '()))

;; Routines for saving/restoring generic variables

; Get a list of values for the variables we wish to save.
(define-public save-variables
  (lambda (names)
    (if (equal? '() names)
        '()
        (cons (eval (list 'copy-tree (car names)) (current-module))
              (save-variables (cdr names))))))

; Restore all the state variables for a game
(define-public restore-variables
  (lambda (names values)
    (or (equal? '() names)
        (begin
          (eval (list 'set! (car names) (list 'quote (car values))) (current-module))
          (restore-variables (cdr names) (cdr values))
          ))))
