; Aisleriot - odessa.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>
;                    Felix Bellaby <felix@pooh.u-net.com>
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


;set up the deck
(set-ace-low)


(define (new-game) 
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK)			;Slot 0
  (add-extended-slot '() down)		;Slot 1
  (add-extended-slot '() down)		;Slot 2
  (add-extended-slot '() down)		;Slot 3
  (add-extended-slot '() down)		;Slot 4
  (add-extended-slot '() down)		;Slot 5
  (add-extended-slot '() down)		;Slot 6
  (add-extended-slot '() down)		;Slot 7
  (add-carriage-return-slot)
  (add-normal-slot '())			;Slot 8
  (add-carriage-return-slot)
  (add-normal-slot '())			;Slot 9
  (add-carriage-return-slot)
  (add-normal-slot '())			;Slot 10
  
  (deal-cards 0 '(1 2 3 4 5 6 7 1 2 3 4 5 6 7  1 2 3 4 5 6 7 ))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 1 2 3 4 5 6 7  1 2 3 4 5 6 7 2 3 4 5 6 2 3 4 5 6))
)


(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)
       (< slot-id 8)
       (not (null? card-list))
       (is-visible? (car (reverse card-list)))))

(define (num-in-a-row value suit rest)
  (if (and (not (null? rest)) 
	   (eq? suit (get-suit (car rest)))
	   (eq? 1 (abs (- value (get-value (car rest))))))
      (+ 1 (num-in-a-row (get-value (car rest)) suit (cdr rest)))
      0))

(define (complete-transaction start-slot card-list rcards end-slot)
  (if (not (empty-slot? end-slot)) ;prevents earning easy points moving kings!
      (let* ((cards (get-cards end-slot))
	     (value (get-value (car cards)))
	     (suit  (get-suit  (car cards))))
	(add-to-score! (+ 1
			  (num-in-a-row value suit (cdr cards))
			  (num-in-a-row value suit rcards)))))
  (move-n-cards! start-slot end-slot card-list)
  (if (not (empty-slot? start-slot)) 
      (make-visible-top-card start-slot))
  #t)

(define (button-released start-slot cards end-slot)
  (and (not (= start-slot end-slot))
       (let ((rcards (reverse cards)))
	 (if (and (> end-slot 0) (< end-slot 8))
	     (and (if (empty-slot? end-slot)
		      (= king (get-value (car rcards)))
		      (and (= (get-suit (get-top-card end-slot))
			      (get-suit (car rcards)))
			   (= (get-value (get-top-card end-slot))
			      (+ (get-value (car rcards)) 1))))
		  (complete-transaction start-slot cards rcards end-slot))
	     (and (if (empty-slot? end-slot)
		      (= ace (get-value (car cards)))
		      (and (= (get-suit (get-top-card end-slot))
			      (get-suit (car cards)))
			   (= (get-value (get-top-card end-slot))
			      (- (get-value (car cards)) 1))))
		  (check-same-suit-list cards)
		  (check-straight-descending-list cards)
		  (complete-transaction start-slot rcards cards end-slot))))))

(define (button-clicked slot)
  #f)
	      
(define (button-double-clicked slot)
  #f)

(define (is-ploppable card value suit)
  (or (and (= ace (get-value card))
	   (list 2 (get-name card) "an empty slot" ))
      (and (or (and (not (empty-slot? 0))
		    (= value (get-value (get-top-card 0)))
		    (= suit (get-suit (get-top-card 0))))
	       (and (not (empty-slot? 8))
		    (= value (get-value (get-top-card 8)))
		    (= suit (get-suit (get-top-card 8))))
	       (and (not (empty-slot? 9))
		    (= value (get-value (get-top-card 9)))
		    (= suit (get-suit (get-top-card 9))))
	       (and (not (empty-slot? 10))
		    (= value (get-value (get-top-card 10)))
		    (= suit (get-suit (get-top-card 10)))))
	   (list 1 (get-name card) (get-name (make-card value suit))))))

(define (is-visible-card cards card value suit)
  (and (not (null? cards))
       (if (and (= (get-value (car cards)) value)
		(= (get-suit (car cards)) suit))
	   (and (is-visible? (car cards))
		(list 1 (get-name (make-card value suit)) (get-name card)))
	   (is-visible-card (cdr cards) card value suit))))

(define (is-extendable slot-id2 slot-id card value suit)
  (and (< slot-id2 8)
       (or (and (not (= slot-id2 slot-id))
		(is-visible-card (get-cards slot-id2) card value suit))
	   (is-extendable (+ 1 slot-id2) slot-id card value suit))))

(define (is-visible-king cards)
  (and (not (null? cards))
       (or (and (= (get-value (car cards)) king)
		(is-visible? (car cards))
		(not (null? (cdr cards)))
		(list 2 (get-name (car cards)) "an empty slot"))
	   (is-visible-king (cdr cards)))))

(define (find-king slot-id)
  (and (< slot-id 8)
       (or (is-visible-king (get-cards slot-id))
	   (find-king (+ 1 slot-id)))))

(define (game-over-helper slot-id check-kings)
  (and (< slot-id 8)
       (or (if (empty-slot? slot-id)
	       (or (and check-kings (find-king 1))
		   (game-over-helper (+ 1 slot-id) #f))		   
	       (let* ((card (get-top-card slot-id))
		      (suit (get-suit card))
		      (value (- (get-value card) 1)))
		   (or (is-ploppable card value suit)
		       (is-extendable 1 slot-id card value suit))))
	   (game-over-helper (+ 1 slot-id) check-kings))))

(define (game-over ugh)
  (game-over-helper 1 #t))

(define (game-won ugh)
  (and (= 13 (list-length (get-cards 0)))
       (= 13 (list-length (get-cards 8)))
       (= 13 (list-length (get-cards 9)))
       (= 13 (list-length (get-cards 10)))))

(define (get-hint ugh)
  (game-over ugh))

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)
