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
  (list 7 3)
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
  (if (and (> start-slot 1)
	   (< start-slot 6))
      (add-to-score! -1))
  (if (and (> end-slot 1)
	   (< end-slot 6))
      (add-to-score! 1))
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

(define (place-ace card slot)
  (remove-card slot)
  (if (empty-slot? 2)
      (complete-transaction slot (list card) 2)
      (if (empty-slot? 3)
	  (complete-transaction slot (list card) 3)
	  (if (empty-slot? 4)
	      (complete-transaction slot (list card) 4)
	      (complete-transaction slot (list card) 5)))))


(define (button-double-clicked slot)
  (if (and (or (> slot 5) (eq? slot 1))
	   (not (empty-slot? slot)))
      (let ((top-card (get-top-card slot)))
	(if (eq? (get-value top-card) ace)
	    (place-ace top-card slot)
	    (if (and (not (empty-slot? 2))
		     (eq? (- (get-value top-card) 1) (get-value (get-top-card 2)))
		     (eq? (get-suit top-card) (get-suit (get-top-card 2))))
		(begin 
		  (remove-card slot)
		  (complete-transaction slot (list top-card) 2))
		(if
		 (and (not (empty-slot? 3))
		      (eq? (- (get-value top-card) 1) (get-value (get-top-card 3)))
		      (eq? (get-suit top-card) (get-suit (get-top-card 3))))
		 (begin 
		   (remove-card slot)
		   (complete-transaction slot (list top-card) 3))
		 (if
		  (and (not (empty-slot? 4))
		       (eq? (- (get-value top-card) 1) (get-value (get-top-card 4)))
		       (eq? (get-suit top-card) (get-suit (get-top-card 4))))
		  (begin 
		    (remove-card slot)
		    (complete-transaction slot (list top-card) 4))
		  (if
		   (and (not (empty-slot? 5))
			(eq? (- (get-value top-card) 1) (get-value (get-top-card 5)))
			(eq? (get-suit top-card) (get-suit (get-top-card 5))))
		   (begin 
		     (remove-card slot)
		     (complete-transaction slot (list top-card) 5))
		   #f)))))
	   #f)))


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

(define (deal-possible?)

(display "deal-possible?\n")

  (if (not (empty-slot? 0))
      (list 0 "Deal a new card from the deck")
      (if (and (< FLIP-COUNTER 3)
	       (not (empty-slot? 1)))
	  (list 0 "Move waste back to stock")
	  #f)))

(define (move-up? card slot)

(display "move-up?\n")
(display "  slot #")
(display slot)
(display "\n")

  (or (if (empty-slot? slot)
	  (begin
	    (display "mu: first clause  ")
	    (display card)
	    (display "\n")
	    (if (= (get-value card)
		   ace)
		(begin
		  (display "mu: in if  \n")
		  (list 1 (get-name card) "empty slot on foundation"))
		(if (< slot 5)
		    (move-up? card (+ 1 slot))
		    #f)))
	  (and (display "mu: checking and\n")
	       (= (get-suit card)
		  (get-suit (get-top-card slot)))
	       (= (get-value card)
		  (+ 1 (get-value (get-top-card slot))))
	       (display "before listing ")
	       (display card)
	       (display "  ")
	       (display (get-top-card slot))
	       (display "\n")
	       (list 2 (get-name card)
		     (get-name (get-top-card slot)))))
      (if (< slot 5)
	  (move-up? card (+ 1 slot))
	  #f)))

(define (get-valid-move check-list)

(display "get-valid-move\n")

(and (not (null? check-list))
     (display "  gvm:  after null check\n")
     (or (and (not (empty-slot? (car check-list)))
	      (display "  gvm:  after not mt-slot cl\n")
	      (move-up? (get-top-card (car check-list)) 2)
;	      (display "  gvm:  after move-up call  ")
;	      (display check-list)
;	      (display "\n")
	      
)
	 (get-valid-move (cdr check-list)))
     ))


(define (tabled card slot)

(display "tabled\n")

  (or (if (and (empty-slot? slot)
	       (display "t: 1\n")
	       (= (get-value card) king)
	       (display "t: 2\n"))
	  (list 1 (get-name card) "empty space on tableau")
	  (and (display "t: 3\n")
	       (eq? (is-black? card)
		    (is-red? (get-top-card slot)))
	       (display "t: 4\n")
	       (= (get-value card)
		  (- (get-value (get-top-card slot)) 1))
	       (display "t: 5\n")
	       (list 2 (get-name card)
		     (get-name (get-top-card slot)))))
      (if (< slot 12)
	  (begin
	    (display "t: 6\n")
	    (tabled card (+ 1 slot)))
	  #f)))

(define (to-tableau? check-list)

(display "to-tableua\n")

  (and (not (null? check-list))
       (or (and (not (empty-slot? (car check-list)))
		(tabled (get-top-card (car check-list)) 6))
	   (to-tableau? (cdr check-list)))))

(define (col-check card start-slot check-slot check-slot-list)

  (display "col-check  ")
  (display card)
  (display "  ")
  (display (get-value card))
  (display king)
  (display "  ")
  (display start-slot)
  (display "  ")
  (display check-slot)
  (display "  ")
  (display check-slot-list)
  (display "\n")
      
  (if (and (empty-slot? check-slot)
	   (< check-slot 12))
      (col-check card 
		 start-slot
		 (+ 1 check-slot) 
		 (get-cards (+ 1 check-slot)))
      (begin
	(or
	 (and (display "cc: 1st and\n")
	      (empty-slot? check-slot)
	      (display "  cc: after empty\n")
	      (= (get-value card) king)
	      (display "  cc: after king\n")
	      (list 1 (get-name card) "empty space on tableau")
	 (and (display "cc: 2nd and\n")
	      (not (= start-slot check-slot))
	      (display "  cc: after not =\n")
	      (eq? (is-black? card)
		   (is-red? (car check-slot-list)))
	      (display "  cc: after color check\n")
	      (= (get-value card)
		 (- (get-value (car check-slot-list)) 1))
	      (display "  cc: after #check\n")
	      (list 2 (get-name card)
		    (get-name (car check-slot-list))))
	 (and (display "cc: 3rd and\n")
	      (< check-slot 12)
	      (col-check card 
			 start-slot
			 (+ 1 check-slot) 
			 (get-cards (+ 1 check-slot)))))))))

(define (move-column? check-list check-list-cards)
  
  (display "move-column?  ")
  (display check-list)
  (display "  ")
  (display check-list-cards)
  (display "\n")
;   (display "  looking ahead:  next:  ")
;   (display (cdr check-list))
;   (display "  ")
;   (display (get-cards (car (cdr check-list))))
;   (display "\n")


  (if (and (not (null? check-list))
	   (if (not (null? check-list-cards))
	       (if (and (display "mc: 1\n")
			(not (is-visible? (car check-list-cards)))
			(display "mc: 2\n"))
		   (move-column? check-list (cdr check-list-cards))    
		   (and (display "mc: checking...:  ")
			(display check-list-cards)
			(display "  backwords:  ")
			(display (reverse check-list-cards))
			(display "  for  ")
			(display (car (reverse check-list-cards)))
			(display "\n")
			(col-check (car check-list-cards)
				   (car check-list) 
				   6
				   (get-cards 6))))
	       #f))
      (move-column? (cdr check-list) (reverse (get-cards (car (cdr check-list)))))))

(define (move-col-reverser check-list list-tbrev)
  (if (not (null? list-tbrev))
      (move-column? check-list list-tbrev)
      (move-col-reverser (cdr check-list) (get-cards (car (cdr check-list))))))

(define (get-hint ugh)
  (or (get-valid-move '(1 6 7 8 9 10 11 12))
      (and (display "after get-valid-move\n")
	   (move-col-reverser '(6 7 8 9 10 11 12) (get-cards 6)))
      (and (display "after move-column\n")
	   (to-tableau? '(1)))
      (deal-possible?)
      (list 0 "Try rearranging the cards")))

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)

