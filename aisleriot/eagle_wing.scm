; AisleRiot - eagle_wing.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>
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

(define BASE-VAL 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())                 ;waste

  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)
  (set! VERTPOS (+ VERTPOS 100))
  (add-extended-slot '() down)          ;tableau (slot 6)
  (set! VERTPOS (- VERTPOS 20))
  (add-extended-slot '() down)          ;tableau (slot 7)
  (set! VERTPOS (- VERTPOS 20))
  (add-extended-slot '() down)          ;tableau (slot 8)
  (set! VERTPOS (+ VERTPOS 20))
  (add-extended-slot '() down)          ;tableau (slot 9)
  (set! VERTPOS (+ VERTPOS 50))
  (add-normal-slot '())                 ;reserve (slot 10)
  (set! VERTPOS (- VERTPOS 50))
  (add-extended-slot '() down)          ;tableau (slot 11)
  (set! VERTPOS (- VERTPOS 20))
  (add-extended-slot '() down)          ;tableau (slot 12)
  (set! VERTPOS (+ VERTPOS 20))
  (add-extended-slot '() down)          ;tableau (slot 13)
  (set! VERTPOS (+ VERTPOS 20))
  (add-extended-slot '() down)          ;tableau (slot 14)

  (deal-cards-face-up 0 '(10))
  (deal-cards 0 '(10 10 10 10 10 10 10 10 10 10 10 10 6 7 8 9 11 12 13 14 2))

  (flip-top-card 2)
  (flip-top-card 6)
  (flip-top-card 7)
  (flip-top-card 8)
  (flip-top-card 9)
  (flip-top-card 11)
  (flip-top-card 12)
  (flip-top-card 13)
  (flip-top-card 14)

  (add-to-score! 1)
  (set! BASE-VAL (get-value (get-top-card 2)))

  (give-status-message)

  (list 9 3))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-reserve-no-string)
					"   "
					(get-base-string)
					"   "
					(get-redeals-string))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (get-reserve-no-string)
  (string-append "Reserve left:  " 
		 (number->string (length (get-cards 10)))))

(define (get-base-string)
  (cond ((and (> BASE-VAL 1)
	      (< BASE-VAL 11))
	 (string-append "Base Card:  " (number->string BASE-VAL)))
	((= BASE-VAL 1)
	 "Base Card:  Ace")
	((= BASE-VAL 11)
	 "Base Card:  Jack")
	((= BASE-VAL 12)
	 "Base Card:  Queen")
	((= BASE-VAL 13)
	 "Base Card:  King")
	(#t #f)))

(define (get-redeals-string)
  (string-append "Redeals left:  "
		 (number->string (- 2 FLIP-COUNTER))))

(define (button-pressed slot-id card-list)
  (and card-list
       (not (member slot-id '(2 3 4 5)))
       (is-visible? (car card-list))))

(define (complete-transaction start-slot card-list end-slot)
  (if (member end-slot '(2 3 4 5))
      (add-to-score! (length card-list)))
  (move-n-cards! start-slot end-slot card-list)
  (if (and (not (= start-slot 1))
	   (empty-slot? start-slot)
	   (not (empty-slot? 10)))
      (deal-cards-face-up 10 (cons start-slot '())))
  (give-status-message))

(define (button-released start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (member end-slot '(2 3 4 5))
		(if (empty-slot? end-slot)
		    (= (get-value (car card-list)) BASE-VAL)
		    (and (eq? (get-suit (car card-list))
			      (get-suit (get-top-card end-slot)))
			 (or (= (get-value (car card-list))
				(+ (get-value (get-top-card end-slot)) 1))
			     (and (= (get-value (car card-list)) ace)
				  (= (get-value (get-top-card end-slot)) 
				     king)))))
		(complete-transaction start-slot (reverse card-list) end-slot))
	   (and (member end-slot '(6 7 8 9 11 12 13 14))
		(= (length card-list) 1)
		(or (empty-slot? end-slot)
		    (and (< (length (get-cards end-slot)) 3)
			 (eq? (get-suit (car card-list))
			      (get-suit (get-top-card end-slot)))
			 (or (= (get-value (car card-list))
				(- (get-value (get-top-card end-slot)) 1))
			     (and (= (get-value (car card-list)) king)
				  (= (get-value (get-top-card end-slot)) 
				     ace)))))
		(complete-transaction start-slot card-list end-slot)))))

(define (button-clicked slot-id)
  (if (= slot-id 0)
      (begin
	(flip-stock 0 1 2)
	(give-status-message))
      #f))

(define (button-double-clicked slot)
  (if (and (not (empty-slot? slot))
	   (is-visible? (get-top-card slot))
	   (or (= slot 1)
	       (and (> slot 5)
		    (< slot 10))
	       (and (> slot 10))))
      (cond ((and (= BASE-VAL (get-value (get-top-card slot))))
	     (cond ((empty-slot? 2)
		    (begin
		      (deal-cards slot '(2))
		      (add-to-score! 1)))
		   ((empty-slot? 3)
		    (begin
		      (deal-cards slot '(3))
		      (add-to-score! 1)))
		   ((empty-slot? 4)
		    (begin
		      (deal-cards slot '(4))
		      (add-to-score! 1)))
		   (#t
		    (begin 
		      (deal-cards slot '(5))
		      (add-to-score! 1)))))
	    ((and (not (empty-slot? 2))
		  (= (get-suit (get-top-card slot))
		     (get-suit (get-top-card 2))))
	     (if (or (and (= (get-value (get-top-card slot)) ace)
			  (= (get-value (get-top-card 2)) king))
		     (= (get-value (get-top-card slot))
			(+ 1 (get-value (get-top-card 2)))))
		 (begin
		   (deal-cards slot '(2))
		   (add-to-score! 1))
		 #f))
	    ((and (not (empty-slot? 3))
		  (= (get-suit (get-top-card slot))
		     (get-suit (get-top-card 3))))
	     (if (or (and (= (get-value (get-top-card slot)) ace)
			  (= (get-value (get-top-card 3)) king))
		     (= (get-value (get-top-card slot))
			(+ 1 (get-value (get-top-card 3)))))
		 (begin
		   (deal-cards slot '(3))
		   (add-to-score! 1))
		 #f))
	    ((and (not (empty-slot? 4))
		  (= (get-suit (get-top-card slot))
		     (get-suit (get-top-card 4))))
	     (if (or (and (= (get-value (get-top-card slot)) ace)
			  (= (get-value (get-top-card 4)) king))
		     (= (get-value (get-top-card slot))
			(+ 1 (get-value (get-top-card 4)))))
		 (begin
		   (deal-cards slot '(4))
		   (add-to-score! 1))
		 #f))
	    ((and (not (empty-slot? 5))
		  (= (get-suit (get-top-card slot))
		     (get-suit (get-top-card 5))))
	     (if (or (and (= (get-value (get-top-card slot)) ace)
			  (= (get-value (get-top-card 5)) king))
		     (= (get-value (get-top-card slot))
			(+ 1 (get-value (get-top-card 5)))))
		 (begin
		   (deal-cards slot '(5))
		   (add-to-score! 1))
		 #f))
	    (#t #f))
      #f)
  (if (and (> slot 5)
	   (not (= slot 10))
	   (empty-slot? slot)
	   (not (empty-slot? 10)))
      (deal-cards-face-up 10 (cons slot '()))))

(define (game-over)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 9)
       (empty-slot? 10)
       (empty-slot? 11)
       (empty-slot? 12)
       (empty-slot? 13)
       (empty-slot? 14)))

(define (check-a-foundation slot1 slot2)
  (and (not (empty-slot? slot2))
       (= (get-suit (get-top-card slot1))
	  (get-suit (get-top-card slot2)))
       (or (= (get-value (get-top-card slot1))
	      (+ 1 (get-value (get-top-card slot2))))
	   (and (= (get-value (get-top-card slot1)) ace)
		(= (get-value (get-top-card slot2)) king)))))

(define (check-to-foundation slot)
  (if (and (not (empty-slot? slot))
	   (is-visible? (get-top-card slot)))
      (cond ((= (get-value (get-top-card slot)) BASE-VAL)
	     (list 3 (get-name (get-top-card slot)) "to an empty foundation"))
	    ((check-a-foundation slot 2)
	     (list 1 
		   (get-name (get-top-card slot)) 
		   (get-name (get-top-card 2))))
	    ((check-a-foundation slot 3)
	     (list 1 
		   (get-name (get-top-card slot)) 
		   (get-name (get-top-card 3))))
	    ((check-a-foundation slot 4)
	     (list 1 
		   (get-name (get-top-card slot)) 
		   (get-name (get-top-card 4))))
	    ((check-a-foundation slot 5)
	     (list 1 
		   (get-name (get-top-card slot)) 
		   (get-name (get-top-card 5))))
	    ((= slot 1)
	     (check-to-foundation 6))
	    ((< slot 14)
	     (check-to-foundation (+ 1 slot)))
	    (#t #f))
      (if (= slot 1)
	  (check-to-foundation 6)
	  (if (< slot 14)
	      (check-to-foundation (+ 1 slot))
	      #f))))

(define (check-empty-slot slot)
  (if (and (empty-slot? slot)
	   (not (= slot 10)))
      (if (empty-slot? 1)
	  #f
	  (list 2 (get-name (get-top-card 1)) "an empty slot on tableau"))
      (if (< slot 14)
	  (check-empty-slot (+ 1 slot))
	  #f)))

(define (check-to-tableau slot card check-slot)
  (if (and (not (= slot check-slot))
	   (not (= check-slot 10))
	   (not (empty-slot? check-slot))
	   (< (length (get-cards check-slot)) 3)
	   (= (get-suit card)
	      (get-suit (get-top-card check-slot)))
	   (or (= (+ 1 (get-value card))
		  (get-value (get-top-card check-slot)))
	       (and (= (get-value card) king)
		    (= (get-value (get-top-card check-slot)) ace))))
      (list 1 (get-name card) (get-name (get-top-card check-slot)))
      (if (< check-slot 14)
	  (check-to-tableau slot card (+ 1 check-slot))
	  #f)))

(define (check-tableau slot)
  (if (and (not (empty-slot? slot))
	   (or (= slot 1)
	       (and (> slot 5)
		    (is-visible? (get-top-card slot))
		    (= 1 (length (get-cards slot))))))
      (check-to-tableau slot (get-top-card slot) 6)
      #f))

(define (dealable?)
  (if (not (empty-slot? 0))
      (list 0 "Deal a card")
      (if (and (not (empty-slot? 1))
	       (< FLIP-COUNTER 2))
	  (list 0 "Move waste back to stock")
	  #f)))

(define (get-hint)
  (or (check-to-foundation 1)
      (check-empty-slot 6)
      (check-tableau 1)
      (check-tableau 6)
      (check-tableau 7)
      (check-tableau 8)
      (check-tableau 9)
      (check-tableau 10)
      (check-tableau 11)
      (check-tableau 12)
      (check-tableau 13)
      (check-tableau 14)
      (dealable?)))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
