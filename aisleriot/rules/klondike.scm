; Aisleriot - klondike.scm
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

(define FLIP-COUNTER 0)

(define (new-game)
  (initialize-playing-area)

					;set up the cards
  (make-standard-deck)
  (shuffle-deck)
  
					;set up the board
  (add-normal-slot DECK)
;  (add-partially-extended-slot '() right 3)
  (add-normal-slot '())
  (add-blank-slot)
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

					;deal the cards
  (deal-cards 0 '(6 7 8 9 10 11 12 7 8 9 10 11 12 8 9 10 11 12 9 10 11 12 10 11 12 11 12 12))
  
  (flip-top-card 6)
  (flip-top-card 7)
  (flip-top-card 8)
  (flip-top-card 9)
  (flip-top-card 10)
  (flip-top-card 11)
  (flip-top-card 12)
  (set! FLIP-COUNTER 0)
)

;Set up the rules
(define (button-pressed slot-id card-list)
  (if (= slot-id 0)
      #f
      (if card-list
	  (if (is-visible? (car (reverse card-list)))
	      #t
	      #f)
	  #f)))
  

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (and (not (empty-slot? start-slot)) (> start-slot 5))
		(make-visible-top-card start-slot)
		#f)
  #t)

(define (button-released start-slot card-list end-slot)
  (if (= start-slot end-slot)
      #f
      (if (empty-slot? end-slot)
	  (if (and (> end-slot 1) (< end-slot 6) (= 1 (list-length card-list)) (= ace (get-value (car card-list))))
	      (complete-transaction start-slot card-list end-slot)
	      (if (and (> end-slot 5) (= king (get-value (car (reverse card-list)))))
		  (complete-transaction start-slot card-list end-slot)
		  #f))
	  (if (and (> end-slot 5)
		   (eq? (is-red? (get-top-card end-slot))
			(is-black? (car (reverse card-list)))))
	      (if (= (get-value (get-top-card end-slot))
		     (+ (get-value (car (reverse card-list))) 1))
		  (complete-transaction start-slot card-list end-slot)
		  #f)
	      (if (and (> end-slot 1) (< end-slot 6) (= 1 (list-length card-list))
		       (= (+ 1 (get-value(get-top-card end-slot))) (get-value (car card-list)))
		       (= (get-suit(get-top-card end-slot)) (get-suit (car card-list))))
		  (complete-transaction start-slot card-list end-slot)
		  #f)))))

  
(define (flip-cards-back)
  (if (> FLIP-COUNTER 2)
		#f
		(if (empty-slot? 1)
			 #f
			 (begin
				(add-card! 0 (flip-card (remove-card 1)))
				(flip-cards-back)))))

  
				  

(define (button-clicked slot-id)
  (if (= slot-id 0)
      (if (empty-slot? 0)
	  (begin
	    (flip-cards-back)
	    (set! FLIP-COUNTER (+ 1 FLIP-COUNTER)))
	  (let ((top-card (remove-card 0)))
	    (if (eq? top-card '())
		#f
		(add-card! 1 (flip-card top-card)))))
      #f))


(define (button-double-clicked slot)
  (if (and (or (> slot 5) (eq? slot 1))
	   (not (empty-slot? slot)))
      (let ((top-card (get-top-card slot)))
	(if (and (eq? (- (get-value top-card) 1) (get-value (get-top-card 2)))
		 (eq? (get-suit top-card) (get-suit (get-top-card 2))))
	    (begin 
	      (remove-card slot)
	      (complete-transaction slot (list top-card) 2))
	    (if
	     (and (eq? (- (get-value top-card) 1) (get-value (get-top-card 3)))
		  (eq? (get-suit top-card) (get-suit (get-top-card 3))))
	     (begin 
	       (remove-card slot)
	       (complete-transaction slot (list top-card) 3))
	     (if
	      (and (eq? (- (get-value top-card) 1) (get-value (get-top-card 4)))
		   (eq? (get-suit top-card) (get-suit (get-top-card 4))))
	     (begin 
	       (remove-card slot)
	       (complete-transaction slot (list top-card) 4))
	      (if
	       (and (eq? (- (get-value top-card) 1) (get-value (get-top-card 5)))
		    (eq? (get-suit top-card) (get-suit (get-top-card 5))))
	     (begin 
	       (remove-card slot)
	       (complete-transaction slot (list top-card) 5))
	       #f)))))
      #f))


(define (game-over ugh)
  (if (and (= 13 (list-length (get-cards 2)))
			  (= 13 (list-length (get-cards 3)))
			  (= 13 (list-length (get-cards 4)))
			  (= 13 (list-length (get-cards 5))))
		#f
		#t))


(define (game-won ugh)
  (if (and (= 13 (list-length (get-cards 2)))
			  (= 13 (list-length (get-cards 3)))
			  (= 13 (list-length (get-cards 4)))
			  (= 13 (list-length (get-cards 5))))
		#t
		#f))

(define (get-hint ugh)
  #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)

