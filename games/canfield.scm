	; AisleRiot - canfield.scm
; Copyright (C) 1998, 2003 Rosanna Yuen <rwsy@mit.edu>
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

(use-modules (aisleriot interface) (aisleriot api))

(define BASE-VAL 0)

(define (new-game)
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)         ; first row
  (add-partially-extended-slot '() right 3 'waste)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (add-normal-slot '() 'reserve)               ; second row
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

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
					(get-base-string))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards 0)))))

(define (get-reserve-no-string)
  (string-append (G_"Reserve left:") " "
		 (number->string (length (get-cards 6)))))

(define (get-base-string)
  (cond ((and (> BASE-VAL 1)
	      (< BASE-VAL 11))
	 (string-append (G_"Base Card: ") (number->string BASE-VAL)))
	((= BASE-VAL 1)
	 (G_"Base Card: Ace"))
	((= BASE-VAL 11)
	 (G_"Base Card: Jack"))
	((= BASE-VAL 12)
	 (G_"Base Card: Queen"))
	((= BASE-VAL 13)
	 (G_"Base Card: King"))
	(#t "")))

(define (button-pressed slot-id card-list)
  (cond ((= slot-id 0)
	 #f)
	((and (= slot-id 1) (> (length card-list) 1))
	 #f)
        ((and (> slot-id 1)
              (< slot-id 6)
              (= BASE-VAL (get-value (car card-list))))
         #f)
	(else
	 (and card-list
	      (is-visible? (car (reverse card-list)))))))

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
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (empty-slot? end-slot)
		    (> end-slot 2) 
		    (< end-slot 6) 
		    (= 1 (length card-list))
		    (= BASE-VAL (get-value (car card-list))))
	   (and (empty-slot? end-slot)
		(> end-slot 6))
	   (and (> end-slot 6)
		(eq? (is-red? (get-top-card end-slot))
		     (is-black? (car (reverse card-list))))
		(or (= (get-value (get-top-card end-slot))
		       (+ (get-value (car (reverse card-list))) 1))
		    (and (= (get-value (get-top-card end-slot)) ace)
	                 (= (get-value (car (reverse card-list))) king) )))
	   (and (> end-slot 1) 
		(< end-slot 6)
		(not (empty-slot? end-slot))
		(= 1 (length card-list))
		(= (get-suit (get-top-card end-slot))
		   (get-suit (car card-list)))
		(or (= (get-value (get-top-card end-slot))
		       (- (get-value (car card-list)) 1))
		    (and (= (get-value (get-top-card end-slot)) king)
			   (= (get-value (car card-list)) ace)))) )))

(define (dealable?)
  (flippable? 0 1 -1))

(define (do-deal-next-cards)
  (flip-stock 0 1 -1 3))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (do-deal-next-cards)))

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
  (and (or (> slot 5) (eq? slot 1))
       (not (empty-slot? slot))
       (let ((top-card (get-top-card slot)))
         (if (eq? (get-value top-card) BASE-VAL)
             (place-ace top-card slot)
             (place-found slot top-card 2)))))

(define (game-over)
  (and (or (get-valid-move '(6 7 8 9 10 1))
	   (to-tableau? '(6 1))
	   (move-column? '(7 8 9 10))
	   (deal-possible?))
       (give-status-message)
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
      (list 0 (G_"Deal a new card from the deck"))
      (list 0 (G_"Move waste back to stock"))))

(define (move-up? from-slot card slot)
  (or (if (empty-slot? slot)
	  (if (= (get-value card)
		 BASE-VAL)
	      (hint-move from-slot 1 slot)
	      #f)
	  (and (= (get-suit card)
		  (get-suit (get-top-card slot)))
	       (or (and (= (get-value card) ace)
			(= (get-value (get-top-card slot)) king))
		   (= (get-value card)
		      (+ 1 (get-value (get-top-card slot)))))
	       (hint-move from-slot 1 slot)))
      (if (< slot 5)
	  (move-up? from-slot card (+ 1 slot))
	  #f)))

(define (get-valid-move check-list)
  (and (not (null? check-list))
       (or (and (not (empty-slot? (car check-list)))
		(move-up? (car check-list) (get-top-card (car check-list)) 2))
	   (get-valid-move (cdr check-list)))))

(define (tabled from-slot card slot)
  (or (if (empty-slot? slot)
	  (hint-move from-slot 1 slot)
	  (and (eq? (is-black? card)
		    (is-red? (get-top-card slot)))
	       (or (and (= (get-value card) king)
			(= (get-value (get-top-card slot)) ace))
		   (= (get-value card)
		      (- (get-value (get-top-card slot)) 1)))
	       (hint-move from-slot 1 slot)))
      (if (< slot 10)
	  (tabled from-slot card (+ 1 slot))
	  #f)))

(define (to-tableau? check-list)
  (and (not (null? check-list))
       (or (and (not (empty-slot? (car check-list)))
		(tabled (car check-list) (get-top-card (car check-list)) 7))
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
		(hint-move start-slot (length (get-cards start-slot)) check-slot)))
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
      (list 0 (G_"Try rearranging the cards"))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable? dealable?)
