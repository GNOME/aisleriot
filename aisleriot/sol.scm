; Aisleriot - sol.scm
; Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
;
; This game is free software; you can redistribute it and/or
; modify it under the terms of the GNU Library General Public
; License as published by the Free Software Foundation; either
; version 2 of the License, or (at your option) any later version.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Library General Public License for more details.
;
; You should have received a copy of the GNU Library General Public
; License along with this library; if not, write to the Free
; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.



;;card stuff
;;cards are internally represented as a list with three values.  The
;;first value is the value of the card (ie. ten, ace), the second is
;;the suit, and the third is a boolean stating whether it is
;;visible (#t) or not (#f).  By default it is not.

(define jack 11)
(define queen 12)
(define king 13)
(define ace 1)

(define black 0)
(define red 1)

(define club 0)
(define diamond 1)
(define heart 2)
(define spade 3)

(define (set-ace-low)
  (set! ace 1))

(define (set-ace-high)
  (set! ace 14))

;basic card functions
(define (make-card value suit)
  (list value suit #f))

(define (make-visible-card value suit)
  (list value suit #t))

(define (flip-card card)
  (list (car card) (cadr card) (not (caddr card))))

(define (make-visible card)
  (list (get-value card) (get-suit card) #t))

(define (is-visible? card)
  (caddr card))

(define (get-value card) 
  (car card))

(define (get-suit card) 
  (cadr card))

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

(define (get-name card)
  (string-append (get-value-name (get-value card)) " of "(get-suit-name (get-suit card))))

;color handling
(define (get-color card)
  (cond ((eq? (get-suit card) club) black)
	((eq? (get-suit card) spade) black)
	((eq? (get-suit card) heart) red)
	((eq? (get-suit card) diamond) red)
	(#t "Unknown color")))

(define (is-red? card)
  (eq? red (get-color card)))

(define (is-black? card)
  (eq? black (get-color card)))

;;deck stuff
;;
;;The deck is a list of cards.   

(define DECK '())

(define FLIP-COUNTER 0)

(define (make-standard-deck-list-ace-high value suit)
  (if (eq? ace value)
      (if (eq? spade suit)
	  (list (make-card ace spade))
	  (cons (make-card value suit) (make-standard-deck-list-ace-high 2 (+ 1 suit))))
      (cons (make-card value suit) (make-standard-deck-list-ace-high (+ 1 value) suit))))

(define (make-standard-deck-list-ace-low value suit)
  (if (eq? king value)
      (if (eq? spade suit)
	  (list (make-card king spade))
	  (cons (make-card value suit) (make-standard-deck-list-ace-low 1 (+ 1 suit))))
      (cons (make-card value suit) (make-standard-deck-list-ace-low (+ 1 value) suit))))

(define (make-standard-deck)
  (if (= ace 14)
      (set! DECK (make-standard-deck-list-ace-high 2 club))
      (set! DECK (make-standard-deck-list-ace-low ace club))))


(define (make-standard-double-deck)
  (if (= ace 14)
      (set! DECK (append (make-standard-deck-list-ace-high 2 club) (make-standard-deck-list-ace-high 2 club)))
      (set! DECK (append (make-standard-deck-list-ace-low ace club) (make-standard-deck-list-ace-low ace club)))))


(define (shuffle-deck)
  (let ((TDECK (list->vector DECK)))
    (set! DECK (vector->list (shuffle-deck-helper TDECK 1 (- (vector-length TDECK) 0))))))

(define (shuffle-deck-helper deck num len)
  (if (= num len)
      deck
      (let ((val-at-num (vector-ref deck num)))
	(let ((replacement-slot (random len)))
	  (vector-set! deck num (vector-ref deck replacement-slot))
	  (vector-set! deck replacement-slot val-at-num))
	(shuffle-deck-helper deck (+ 1 num) len))))

(define (flip-cards-back flip-number)
  (if (> FLIP-COUNTER flip-number)
      #f
      (if (empty-slot? 1)
	  #f
	  (begin
	    (add-card! 0 (flip-card (remove-card 1)))
	    (flip-cards-back flip-number)))))


;;playing area stuff
;;
;;The playing area is divided into slots, where cards can be placed.
;;Each slot can hold any amount of cards.  The slots themselves have
;;an identifying tag, a deck of the cards they contain, and an
;;(optional) location value, for placement on the playing area.  The
;;order that the slots are added to the playing area is not
;;necesserily important to their placement, but it does determine it
;;in the default case.  
;;
;;Note: Once a slot is placed, it loses it's location information.


(define SLOTS 0)
(define HORIZPOS 0)
(define VERTPOS 0)

(define down 0)
(define right 1)
;internal representation of a slot:
;  '(tag list-of-cards placement-variable)

(define (initialize-playing-area)
  (reset-surface)
  (set! SLOTS 0)
  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (get-vert-start)))


;; Slot functions
(define (new-slot deck placement)
  (list #f deck placement))

(define (set-tag! slot)
  (set! SLOTS (+ 1 SLOTS))
  (cons (- SLOTS 1) (cdr slot)))

(define (get-cards slotval)
  (cadr (if (pair? slotval)
	    slotval
	    (get-slot slotval))))

(define (add-card! slot-id card)
  (let ((slot (get-slot slot-id)))
    (set-cards! slot-id (cons card (get-cards slot)))))

(define (add-cards! slot-id cards)
  (let ((slot (get-slot slot-id)))
    (set-cards! slot-id (append cards (get-cards slot)))))

(define (cards-eq? card1 card2)
  (and (eq? (get-value card1) (get-value card2))
       (eq? (get-suit card1) (get-suit card2))))

(define (remove-card slot-id)
  (let ((slot (get-slot slot-id)))
    (let ((retval (get-top-card slot-id)))
      (set-cards! slot-id (cdr (car (cdr slot))))
      retval)))

(define (remove-n-cards slot-id n)
  (set-cards! slot-id (remove-n-cards-helper n (get-cards (get-slot slot-id)))))

(define (remove-n-cards-helper n cards)
  (if (eq? '() cards)
      cards
      (if (= n 0)
	  cards
	  (remove-n-cards-helper (- n 1) (cdr cards)))))


;;Surface functions
(define (get-and-increment-position)
  (let ((retval (list HORIZPOS VERTPOS)))
    (set! HORIZPOS (+ HORIZPOS (get-horiz-offset)))
    retval))

(define (linefeed-position)
  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (+ VERTPOS (get-vert-offset))))


; Game calls
(define (add-normal-slot deck)
  (add-slot (set-tag! (new-slot deck (list 'normal (get-and-increment-position))))))

(define (add-extended-slot deck direction)
  (cond ((= right direction)
	 (add-slot (set-tag! (new-slot deck (list 'expanded-right (get-and-increment-position))))))
	(#t
	 (add-slot (set-tag! (new-slot deck (list 'expanded (get-and-increment-position))))))))


(define (add-partially-extended-slot deck direction n)
  (cond ((= right direction)
	 (add-slot (set-tag! (new-slot deck (list 'partially-expanded-right (get-and-increment-position) n)))))
	(#t
	 (add-slot (set-tag! (new-slot deck (list 'partially-expanded (get-and-increment-position) n)))))))


(define (add-blank-slot)
  (get-and-increment-position)
  #t)

(define (empty-slot? slot-id)
  (if (eq? '() (cadr (get-slot slot-id)))
      #t
      #f))

(define (move-n-cards! start-slot end-slot cards)
  (add-cards! end-slot cards))

(define (add-carriage-return-slot)
  (linefeed-position))

(define (deal-cards target-slot-id slot-list)
  (if (eq? '() slot-list)
      '()
      (begin 
	(add-card! (car slot-list) (remove-card target-slot-id))
	(deal-cards target-slot-id (cdr slot-list)))))

(define (deal-cards-face-up target-slot-id slot-list)
  (if (eq? '() slot-list)
      '()
      (begin 
	(add-card! (car slot-list) (make-visible (remove-card target-slot-id)))
	(deal-cards-face-up target-slot-id (cdr slot-list)))))

(define (deal-cards-from-deck deck slot-list)
  (if (eq? '() slot-list)
      '()
      (begin 
	(add-card! (car slot-list) (car deck))
	(deal-cards-from-deck (cdr deck) (cdr slot-list)))))

(define (deal-cards-face-up-from-deck deck slot-list)
  (if (eq? '() slot-list)
      '()
      (begin 
	(add-card! (car slot-list) (make-visible (car deck)))
	(deal-cards-face-up-from-deck (cdr deck) (cdr slot-list)))))

(define (get-top-card slot-id)
  (if (empty-slot? slot-id)
      '()
      (car (cadr (get-slot slot-id)))))

(define (flip-top-card slot-id)
  (add-card! slot-id (flip-card (remove-card slot-id))))

(define (make-visible-top-card slot-id)
  (add-card! slot-id (make-visible (remove-card slot-id))))

(define (check-same-suit-list card-list)
  (if (< (list-length card-list) 2)
      #t
      (if (= (get-suit (car card-list)) (get-suit (cadr card-list)))
	  (check-same-suit-list (cdr card-list))
	  #f)))

(define (check-straight-descending-list card-list)
  (if (< (list-length card-list) 2)
      #t
      (if (= (get-value (car card-list)) (- (get-value (cadr card-list)) 1))
	  (check-straight-descending-list (cdr card-list))
	  #f)))

;not strictly solitaire, but sometimes useful.
(define (display-list . objs)
  (map display objs))

