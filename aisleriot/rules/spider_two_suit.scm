; AisleRiot - spider.scm
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

;set up the deck
(set-ace-low)

; create a deck of 104 cards (made up of four decks of just hearts and spades)
; defined an ace high and ace low in case other games want to make
; use of it. 
; Copythight (C) 2003 Aaron Schlaegel <opensource@schlaegel.com>, GPL
(define (make-twosuit-quadruple-deck)
  (if (= ace 14)
      (set! DECK (append (make-standard-deck-list-ace-high 2 heart) (make-standard-deck-list-ace-high 2 heart) (make-standard-deck-list-ace-high 2 heart) (make-standard-deck-list-ace-high 2 heart)))
      (set! DECK (append (make-standard-deck-list-ace-low ace heart) (make-standard-deck-list-ace-low ace heart) (make-standard-deck-list-ace-low ace heart) (make-standard-deck-list-ace-low ace heart)))))

(define (new-game)
  (initialize-playing-area)
  (make-twosuit-quadruple-deck)
  (shuffle-deck)

  ;set up the board
  (add-normal-slot DECK)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (deal-initial-setup)

  (give-status-message)

  (list 10 4))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

;internal procedures/variables

(define (deal-initial-setup)
  (deal-cards 0 '(9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 12 15 18))
  (flip-top-card 9)
  (flip-top-card 10)
  (flip-top-card 11)
  (flip-top-card 12)
  (flip-top-card 13)
  (flip-top-card 14)
  (flip-top-card 15)
  (flip-top-card 16)
  (flip-top-card 17)
  (flip-top-card 18))

;additional functions.

(define (complete-transaction start-slot card-list end-slot)
  (if (and (not (empty-slot? start-slot))
	   (is-visible? (get-top-card start-slot))
	   (eq? (get-suit (get-top-card start-slot))
		(get-suit (car (reverse card-list))))
	   (= (get-value (get-top-card start-slot))
	      (+ 1 (get-value (car (reverse card-list))))))
      (add-to-score! -1))
  (if (and (not (empty-slot? end-slot))
	   (is-visible? (get-top-card end-slot))
	   (eq? (get-suit (get-top-card end-slot))
		(get-suit (car (reverse card-list))))
	   (= (get-value (get-top-card end-slot))
	      (+ 1 (get-value (car (reverse card-list))))))
      (add-to-score! 1))
  (move-n-cards! start-slot end-slot card-list)
  (if (and (not (empty-slot? start-slot)) (> start-slot 5))
      (make-visible-top-card start-slot)
      #f)
  #t)

(define (check-for-points slot-id)
  (if (> slot-id 18)
      #t
      (begin
	(if (and (is-visible? (cadr (get-cards slot-id)))
		 (eq? (get-suit (get-top-card slot-id))
		      (get-suit (cadr (get-cards slot-id))))
		 (= (+ 1 (get-value (get-top-card slot-id)))
		    (get-value (cadr (get-cards slot-id)))))
	    (add-to-score! 1))
	(check-for-points (+ 1 slot-id)))))

(define (deal-new-cards)
  (if (> (length (get-cards 0)) 0)
      (begin
	(deal-cards-face-up 0 '(9 10 11 12 13 14 15 16 17 18))
	(check-for-points 9))
      #f))

(define (button-pressed slot card-list)
  (give-status-message)
  (if (or (empty-slot? slot)
	  (< slot 9))
      #f
      (if (not (eq? '() card-list))
	  (if (is-visible? (car (reverse card-list)))
	      (if (check-same-suit-list card-list)
		  (if (check-straight-descending-list card-list)
		      #t
		      #f)
		  #f)
	      #f)
	  #f)))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (if (empty-slot? end-slot)
 	   (or (and (> end-slot 0) 
		    (< end-slot 9) 
		    (= 13 (length card-list)))
	       (> end-slot 8))
	   (and (> end-slot 8)
	        (= (get-value (get-top-card end-slot))
	        (+ (get-value (car (reverse card-list))) 1))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (any-slot-empty?)
  (or (empty-slot? 9)
      (empty-slot? 10)
      (empty-slot? 11)
      (empty-slot? 12)
      (empty-slot? 13)
      (empty-slot? 14)
      (empty-slot? 15)
      (empty-slot? 16)
      (empty-slot? 17)
      (empty-slot? 18)))

(define (button-clicked slot)
  (and (= 0 slot)
       (if (any-slot-empty?)
	   (begin
             (set-statusbar-message (_"Please fill in empty pile first."))
             #f)
	   (begin
	     (deal-new-cards)
	     (give-status-message)
	     #t))))

(define (button-double-clicked slot)
  #f)

(define (game-over)
  (and (not (game-won))
       (get-hint)))

; Game can be won on two conditions.  Either all the cards being moved
; to the top slots, or by stacking all the cards (score of 96)
(define (game-won)
  (or
   (and (eq? (get-score) 96)
	(empty-slot? 1)
	(empty-slot? 2)
	(empty-slot? 3)
	(empty-slot? 4)
	(empty-slot? 5)
	(empty-slot? 6)
	(empty-slot? 7)
	(empty-slot? 8))
   (and (empty-slot? 0)
	(empty-slot? 9)
	(empty-slot? 10)
	(empty-slot? 11)
	(empty-slot? 12)
	(empty-slot? 13)
	(empty-slot? 14)
	(empty-slot? 15)
	(empty-slot? 16)
	(empty-slot? 17)
	(empty-slot? 18))))

(define (depth-card card-list)
  (if (and (> (length card-list) 1)
	   (is-visible? (cadr card-list))
	   (eq? (get-suit (car card-list))
		(get-suit (cadr card-list)))
	   (eq? (+ 1 (get-value (car card-list)))
		(get-value (cadr card-list))))
      (depth-card (cdr card-list))
      card-list))

(define (check-a-slot slot1 card-to-move slot2 same-suit?)
  (if (> slot2 18)
      #f
      (if (and (not (= slot1 slot2))
	       (not (empty-slot? slot2))
	       (eq? same-suit?
		    (eq? (get-suit card-to-move)
			 (get-suit (get-top-card slot2))))
	       (= (+ 1 (get-value card-to-move))
		  (get-value (get-top-card slot2))))
	  (list 1
		(get-name card-to-move)
		(get-name (get-top-card slot2)))
	  (check-a-slot slot1 card-to-move (+ 1 slot2) same-suit?))))

(define (same-suit-check slot-id)
  (if (> slot-id 18)
      #f
      (if (and (not (empty-slot? slot-id))
	       (check-a-slot slot-id (car (depth-card (get-cards slot-id))) 9 #t))
	  (check-a-slot slot-id (car (depth-card (get-cards slot-id))) 9 #t)
	  (same-suit-check (+ 1 slot-id)))))

(define (not-same-suit-check slot-id)
  (if (> slot-id 18)
      #f
      (if (and (not (empty-slot? slot-id))
	       (or (= 1 (length (depth-card (get-cards slot-id))))
		   (not (eq? (+ 1 (get-value (car (depth-card (get-cards slot-id)))))
			     (get-value (cadr (depth-card (get-cards slot-id)))))))
	       (check-a-slot slot-id (car (depth-card (get-cards slot-id))) 9 #f))
	  (check-a-slot slot-id (car (depth-card (get-cards slot-id))) 9 #f)
	  (not-same-suit-check (+ 1 slot-id)))))

(define (open-slots? slot-id)
  (if (> slot-id 18)
      #f
      (if (empty-slot? slot-id)
	  (list 0 (_"Place something on empty slot"))
	  (open-slots? (+ 1 slot-id)))))

(define (dealable?)
  (if (not (empty-slot? 0))
      (list 0 (_"Deal another round"))
      #f))

(define (get-hint)
  (or (same-suit-check 9)
      (not-same-suit-check 9)
      (open-slots? 9)
      (dealable?)
; this isn't great, but it will get around the premature end-of-game call
      (list 0 (_"Try moving card piles around"))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
