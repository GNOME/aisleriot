; Aisleriot - spider.scm
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


;set up the deck
(set-ace-low)


(define (new-game)
  (initialize-playing-area)
  (make-standard-double-deck)
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
  (list 10 4)  
)
;internal procedures/variables
(define DEAL_COUNTER 1)

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
  (move-n-cards! start-slot end-slot card-list)
  (if (and (not (empty-slot? start-slot)) (> start-slot 5))
      (make-visible-top-card start-slot)
      #f)
  #t)

(define (deal-new-cards)
  (deal-cards-face-up 0 '(9 10 11 12 13 14 15 16 17 18)))


(define (button-pressed slot card-list)
  (if (empty-slot? slot)
      #f
      (if (< slot 9)
	  #f
	  (if (not (eq? '() card-list))
	      (if (is-visible? (car (reverse card-list)))
		  (if (check-same-suit-list card-list)
		      (if (check-straight-descending-list card-list)
			  #t
			  #f)
		      #f)
		  #f)
	      #f))))

(define (button-released start-slot card-list end-slot)
  (if (= start-slot end-slot)
      #f
      (if (empty-slot? end-slot)
	  (cond ((and (> end-slot 0) 
		      (< end-slot 9) 
		      (= 13 (list-length card-list)))
		 (complete-transaction start-slot card-list end-slot))
		((> end-slot 8) 
		 (complete-transaction start-slot card-list end-slot))
		(#t #f))
	  (cond ((and (> end-slot 8)
		      (= (get-value (get-top-card end-slot))
			 (+ (get-value (car (reverse card-list))) 1)))
		 (complete-transaction start-slot card-list end-slot))
		(#t #f)))))
      


(define (button-clicked slot)
  (if (= 0 slot)
      (begin
	(set! DEAL_COUNTER (+ 1 DEAL_COUNTER))
	(cond ((> DEAL_COUNTER 6 )
	       #f)
	      (#t (deal-new-cards))))
      #f))

	      
(define (button-double-clicked slot)
  #f)
(define (game-over ugh)
  #t)
(define (game-won ugh)
  #f)
(define (get-hint ugh)
  #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)


 
