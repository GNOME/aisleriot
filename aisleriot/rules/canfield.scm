; AisleRiot - canfield.scm
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
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)         ; first row
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-normal-slot '())               ; second row
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards 0 '(6 6 6 6 6 6 6 6 6 6 6 6 6 7 8 9 10 2))

  (flip-top-card 6)
  (flip-top-card 7)
  (flip-top-card 8)
  (flip-top-card 9)
  (flip-top-card 10)
  (flip-top-card 2)

  (set! BASE-VAL (get-value (get-top-card 2)))
  
  (give-status-message)

  (add-to-score! 1)

  (list 7 4)
)

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
		 (number->string (length (get-cards 6)))))

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
  (if (= slot-id 0)
      #f
      (if card-list
	  (if (is-visible? (car (reverse card-list)))
	      (if (and (= slot-id 2)
		       (= (length (get-cards 2)) 1))
		  #f
		  #t)
	      #f)
	  #f)))

(define (complete-transaction start-slot card-list end-slot)
  (if (and (> start-slot 1)
	   (< start-slot 6))
      (begin
	(if (= (get-value (car card-list))
	       BASE-VAL)
	    (if (empty-slot? 3)
		(set! end-slot 3)
		(if (empty-slot? 4)
		    (set! end-slot 4))))
	(add-to-score! -1)))
  (if (and (> end-slot 1)
	   (< end-slot 6))
      (add-to-score! 1))
  (move-n-cards! start-slot end-slot card-list)
  (if (and (empty-slot? start-slot) 
	   (> start-slot 6)
	   (not (empty-slot? 6)))
      (begin 
	(let ((top-card (remove-card 6)))
	  (if (eq? top-card '())
	      #f
	      (add-card! start-slot top-card)))
	(if (not (empty-slot? 6))
	    (make-visible-top-card 6))))
  (if (and (not (empty-slot? start-slot)) 
	   (= start-slot 6))
      (make-visible-top-card start-slot)
      #f)
  #t)

(define (button-released start-slot card-list end-slot)
  (if (= start-slot end-slot)
      #f
      (cond ((and (empty-slot? end-slot)
		  (> end-slot 2) 
		  (< end-slot 6) 
		  (= 1 (length card-list))
		  (= BASE-VAL (get-value (car card-list))))
	     (complete-transaction start-slot card-list end-slot))
	    ((and (empty-slot? end-slot)
		  (> end-slot 6))
	     (complete-transaction start-slot card-list end-slot))
	    ((and (> end-slot 6)
		  (eq? (is-red? (get-top-card end-slot))
		       (is-black? (car (reverse card-list))))
		  (or (= (get-value (get-top-card end-slot))
			 (+ (get-value (car (reverse card-list))) 1))
		      (and (= (get-value (get-top-card end-slot))
			      ace)
			   (= (get-value (car (reverse card-list)))
			      king))))
	     (complete-transaction start-slot card-list end-slot))
	    ((and (> end-slot 1) 
		  (< end-slot 6)
		  (not (empty-slot? end-slot))
		  (= 1 (length card-list))
		  (= (get-suit (get-top-card end-slot))
		     (get-suit (car card-list)))
		  (or (= (get-value (get-top-card end-slot))
			 (- (get-value (car card-list)) 1))
		      (and (= (get-value (get-top-card end-slot))
			      king)
			   (= (get-value (car card-list))
			      ace))))
	     (complete-transaction start-slot card-list end-slot))
	    (else #f))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (flip-stock 0 1 2)))

(define (place-ace card slot)
  (remove-card slot)
  (if (empty-slot? 2)
      (complete-transaction slot (list card) 2)
      (if (empty-slot? 3)
	  (complete-transaction slot (list card) 3)
	  (if (empty-slot? 4)
	      (complete-transaction slot (list card) 4)
	      (complete-transaction slot (list card) 5)))))

(define (place-found slot top-card search)
  (if (and (not (empty-slot? search))
	   (or (eq? (- (get-value top-card) 1) 
		    (get-value (get-top-card search)))
	       (and (eq? (get-value top-card) ace)
		    (eq? (get-value (get-top-card search)) king)))
	   (eq? (get-suit top-card) (get-suit (get-top-card search))))
      (begin 
	(remove-card slot)
	(complete-transaction slot (list top-card) search))
      (if (= search 5)
	  #f
	  (place-found slot top-card (+ search 1)))))

(define (button-double-clicked slot)
  (if (and (or (> slot 5) (eq? slot 1))
	   (not (empty-slot? slot)))
      (let ((top-card (get-top-card slot)))
	(if (eq? (get-value top-card) BASE-VAL)
	    (place-ace top-card slot)
	    (place-found slot top-card 2)))))

(define (game-over)
  (and (give-status-message)
       (not (game-won))))

(define (game-won)
  (if (and (empty-slot? 0)
	   (empty-slot? 1)
	   (empty-slot? 6)
	   (empty-slot? 7)
	   (empty-slot? 8)
	   (empty-slot? 9)
	   (empty-slot? 10))
      #t
      #f))

(define (deal-possible?)
  (if (not (empty-slot? 0))
      (list 0 "Deal a new card from the deck")
      (if (and (< FLIP-COUNTER 3)
	       (not (empty-slot? 1)))
	  (list 0 "Move waste back to stock")
	  #f)))

(define (move-up? card slot)
  (or (if (empty-slot? slot)
	  (if (= (get-value card)
		 BASE-VAL)
	      (list 1 (get-name card) "empty slot on foundation")
	      #f)
	  (and (= (get-suit card)
		  (get-suit (get-top-card slot)))
	       (or (and (= (get-value card) ace)
			(= (get-value (get-top-card slot)) king))
		   (= (get-value card)
		      (+ 1 (get-value (get-top-card slot)))))
	       (list 2 (get-name card)
		     (get-name (get-top-card slot)))))
      (if (< slot 5)
	  (move-up? card (+ 1 slot))
	  #f)))

(define (get-valid-move check-list)
  (and (not (null? check-list))
       (or (and (not (empty-slot? (car check-list)))
		(move-up? (get-top-card (car check-list)) 2))
	   (get-valid-move (cdr check-list)))))

(define (tabled card slot)
  (or (if (empty-slot? slot)
	  (list 1 (get-name card) "empty space on tableau")
	  (and (eq? (is-black? card)
		    (is-red? (get-top-card slot)))
	       (or (and (= (get-value card) king)
			(= (get-value (get-top-card slot)) ace))
		   (= (get-value card)
		      (- (get-value (get-top-card slot)) 1)))
	       (list 2 (get-name card)
		     (get-name (get-top-card slot)))))
      (if (< slot 10)
	  (tabled card (+ 1 slot))
	  #f)))

(define (to-tableau? check-list)
  (and (not (null? check-list))
       (or (and (not (empty-slot? (car check-list)))
		(tabled (get-top-card (car check-list)) 7))
	   (to-tableau? (cdr check-list)))))

(define (col-check card start-slot check-slot)
  (if (> check-slot 10)
      #f
      (or 
       (if (= start-slot check-slot)
	   (col-check card start-slot (+ 1 check-slot))
	   (and (not (empty-slot? check-slot))
		(eq? (is-black? card)
		     (is-red? (get-top-card check-slot)))
		(or (and (= (get-value card) king)
			 (= (get-value (get-top-card check-slot)) ace))
		    (= (get-value card)
		       (- (get-value (get-top-card check-slot)) 1)))
		(list 2 (get-name card)
		      (get-name (get-top-card check-slot)))))
       (col-check card start-slot (+ 1 check-slot)))))

(define (move-column? check-list)
  (and (not (null? check-list))
       (or (and (not (empty-slot? (car check-list)))
		(col-check (car
			    (reverse (get-cards (car check-list)))) 
			   (car check-list) 7))
	   (move-column? (cdr check-list)))))

(define (get-hint)
  (or (get-valid-move '(6 7 8 9 10 1))
      (to-tableau? '(6 1))
      (move-column? '(7 8 9 10))
      (deal-possible?)
      (list 0 "Try rearranging the cards")))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
