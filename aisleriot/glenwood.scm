; AisleRiot - glenwood.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
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
  (add-normal-slot '())

  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-extended-slot '() right)

  (add-blank-slot)
  (add-blank-slot)
  
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)

  (deal-cards-face-up 0 '(6 11 12 13 6 11 12 13 6 11 12 13 7 8 9 10))

  (give-initial-status-message)

  (list 7 5)

)

(define (give-initial-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string)
					"   "
					(get-base-string))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (get-redeals-string)
  (string-append "Redeals left:  "
		 (number->string (- 1 FLIP-COUNTER))))

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

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (not (or (= slot-id 2)
		(= slot-id 3)
		(= slot-id 4)
		(= slot-id 5)))		
       (or (and (not (empty-slot? 2))
		(is-visible? (car card-list))
		(or (= (length card-list) 1)
		    (and (= (length card-list)
			    (length (get-cards slot-id)))
			 (or (= slot-id 7)
			     (= slot-id 8)
			     (= slot-id 9)
			     (= slot-id 10)))))
	   (and (= (length card-list) 1)
		(or (= slot-id 6)
		    (= slot-id 11)
		    (= slot-id 12)
		    (= slot-id 13))))))

(define (button-released start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (= (length card-list) 1)
		(or (and (empty-slot? end-slot)
			 (= end-slot 2)
			 (set! BASE-VAL (get-value (car card-list)))
			 (move-n-cards! start-slot end-slot card-list)
			 (add-to-score! 1)
			 (give-status-message))
		    (and (not (empty-slot? 2))
			 (or (and (empty-slot? end-slot)
				  (or (and (or (= end-slot 3)
					       (= end-slot 4)
					       (= end-slot 5))
					   (= BASE-VAL (get-value (car card-list)))
					   (move-n-cards! start-slot end-slot card-list)
					   (add-to-score! 1))
				      (and (or (empty-slot? start-slot)
					       (not (or (= start-slot 7)
							(= start-slot 8)
							(= start-slot 9)
							(= start-slot 10))))
					   (or (not (= start-slot 1))
					       (and (empty-slot? 6)
						    (empty-slot? 11)
						    (empty-slot? 12)
						    (empty-slot? 13)))
					   (or (= end-slot 7)
					       (= end-slot 8)
					       (= end-slot 9)
					       (= end-slot 10))
					   (move-n-cards! start-slot end-slot card-list))))
			     (and (not (empty-slot? end-slot))
				  (or (= end-slot 2)
				      (= end-slot 3)
				      (= end-slot 4)
				      (= end-slot 5))
				  (= (get-suit (get-top-card end-slot))
				     (get-suit (car card-list)))
				  (or (= (get-value (car card-list))
					 (+ 1 (get-value (get-top-card end-slot))))
				      (and (= (get-value (car card-list))
					      ace)
					   (= (get-value (get-top-card end-slot))
					      king)))
				  (move-n-cards! start-slot end-slot card-list)
				  (add-to-score! 1))))))
	   (and (or (empty-slot? start-slot)
		    (= start-slot 1)
		    (= start-slot 6)
		    (= start-slot 11)
		    (= start-slot 12)
		    (= start-slot 13))
		(or (= end-slot 7)
		    (= end-slot 8)
		    (= end-slot 9)
		    (= end-slot 10))
		(or (and (not (empty-slot? end-slot))
			 (eq? (is-black? (get-top-card end-slot))
			      (is-red? (car (reverse card-list))))
			 (or (= (+ 1 (get-value (car (reverse card-list))))
				(get-value (get-top-card end-slot)))
			     (and (= (get-value (car (reverse card-list)))
				     king)
				  (= (get-value (get-top-card end-slot))
				     ace)))
			 (move-n-cards! start-slot end-slot card-list))
		    (and (empty-slot? end-slot)
			 (or (not (= start-slot 1))
			     (and (empty-slot? 6)
				  (empty-slot? 11)
				  (empty-slot? 12)
				  (empty-slot? 13)))
			 (move-n-cards! start-slot end-slot card-list)))))))
			      
(define (button-clicked slot-id)
  (and (not (empty-slot? 2))
       (= slot-id 0)
       (flip-stock 0 1 1)))

(define (move-to-foundation start-slot card-list end-slot)
  (deal-cards start-slot (list end-slot))
  (add-to-score! 1))

(define (place-ace card slot)
  (if (empty-slot? 2)
      (move-to-foundation slot (list card) 2)
      (if (empty-slot? 3)
	  (move-to-foundation slot (list card) 3)
	  (if (empty-slot? 4)
	      (move-to-foundation slot (list card) 4)
	      (move-to-foundation slot (list card) 5)))))

(define (place-found slot top-card search)
  (if (and (not (empty-slot? search))
	   (or (eq? (- (get-value top-card) 1) 
		    (get-value (get-top-card search)))
	       (and (eq? (get-value top-card) ace)
		    (eq? (get-value (get-top-card search)) king)))
	   (eq? (get-suit top-card) (get-suit (get-top-card search))))
      (begin 
	(move-to-foundation slot (list top-card) search))
      (if (= search 5)
	  #f
	  (place-found slot top-card (+ search 1)))))



(define (button-double-clicked slot-id)
  (if (and (not (or (= slot-id 0)
		    (= slot-id 2)
		    (= slot-id 3)
		    (= slot-id 4)
		    (= slot-id 5)))
	   (not (empty-slot? 2))
	   (not (empty-slot? slot-id)))
      (let ((top-card (get-top-card slot-id)))
	(if (eq? (get-value top-card) BASE-VAL)
	    (place-ace top-card slot-id)
	    (place-found slot-id top-card 2)))
      #f))
		


(define (game-continuable)
  (if (empty-slot? 2)
      (give-initial-status-message)
      (give-status-message))
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))
       (= 13 (length (get-cards 5)))))

(define (dealable?)
  (if (not (empty-slot? 0))
      (list 0 "Deal a new card from the deck")
      (if (and (< FLIP-COUNTER 1)
	       (not (empty-slot? 1)))
	  (list 0 "Move waste back to stock")
	  #f)))

(define (base-not-set?)
  (and (empty-slot? 2)
       (list 0 "Select a card from the reserve for first foundation pile")))

(define (check-a-foundation slot-id foundation-id)
  (cond ((= foundation-id 6)
	 #f)
	((empty-slot? foundation-id)
	 (check-a-foundation slot-id (+ 1 foundation-id)))
	((eq? (get-suit (get-top-card slot-id))
	      (get-suit (get-top-card foundation-id)))
	 (or (= (get-value (get-top-card slot-id))
		(+ 1 (get-value (get-top-card foundation-id))))
	     (and (= (get-value (get-top-card slot-id))
		     ace)
		  (= (get-value (get-top-card foundation-id))
		     king))))
	(#t (check-a-foundation slot-id (+ 1 foundation-id)))))	

(define (to-foundations slot-id)
  (cond ((= slot-id 14)
	 #f)
	((= slot-id 2)
	 (to-foundations 6))
	((and (not (empty-slot? slot-id))
	      (= (get-value (get-top-card slot-id))
		 BASE-VAL))
	 (list 1 (get-name (get-top-card slot-id)) "empty slot on foundation"))
	((and (not (empty-slot? slot-id))
	      (check-a-foundation slot-id 2))
	 (if (= (get-value (get-top-card slot-id)) ace)
	     (list 1 
		   (get-name (get-top-card slot-id))
		   (get-name (make-card king
					(get-suit (get-top-card slot-id)))))
	     (list 1 
		   (get-name (get-top-card slot-id))
		   (get-name (make-card (- (get-value (get-top-card slot-id))
					   1)
					(get-suit (get-top-card slot-id)))))))
	(#t
	 (to-foundations (+ 1 slot-id)))))

(define (check-a-tableau-with-single slot-id tab-id)
  (cond ((= tab-id 11)
	 #f)
	((and (not (empty-slot? tab-id))
	      (eq? (is-red? (get-top-card slot-id))
		   (is-black? (get-top-card tab-id)))
	      (or (= (+ 1 (get-value (get-top-card slot-id)))
		     (get-value (get-top-card tab-id)))
		  (and (= (get-value (get-top-card slot-id)) king)
		       (= (get-value (get-top-card tab-id)) ace))))
	 (list 1 
	       (get-name (get-top-card slot-id)) 
	       (get-name (get-top-card tab-id))))
	(#t (check-a-tableau-with-single slot-id (+ 1 tab-id)))))

(define (check-a-tableau-pile slot-id tab-id)
  (cond ((= tab-id 11)
	 #f)
	((and (not (empty-slot? tab-id))
	      (not (= slot-id tab-id))
	      (eq? (is-red? (car (reverse (get-cards slot-id))))
		   (is-black? (get-top-card tab-id)))
	      (or (= (+ 1 (get-value (car (reverse (get-cards slot-id)))))
		     (get-value (get-top-card tab-id)))
		  (and (= (get-value (car (reverse (get-cards slot-id)))) king)
		       (= (get-value (get-top-card tab-id)) ace))))
	 (list 1 
	       (get-name (car (reverse (get-cards slot-id))))
	       (get-name (get-top-card tab-id))))
	(#t (check-a-tableau-pile slot-id (+ 1 tab-id)))))


(define (to-tableau slot-id)
  (cond ((= slot-id 14)
	 #f)
	((= slot-id 2)
	 (to-tableau 6))
	((and (not (empty-slot? slot-id))
	      (or (= slot-id 1)
		  (= slot-id 6)
		  (> slot-id 10))
	      (check-a-tableau-with-single slot-id 7))
	 (check-a-tableau-with-single slot-id 7))
	((and (not (empty-slot? slot-id))
	      (> slot-id 6)
	      (< slot-id 11)
	      (check-a-tableau-pile slot-id 7))
	 (check-a-tableau-pile slot-id 7))

	(#t (to-tableau (+ 1 slot-id)))))

(define (empty-tableau? slot-id)
  (if (or (empty-slot? 7)
	  (empty-slot? 8)
	  (empty-slot? 9)
	  (empty-slot? 10))
      (cond ((or (not (empty-slot? 6))
		 (not (empty-slot? 11))
		 (not (empty-slot? 12))
		 (not (empty-slot? 13)))
	     (list 0 "Move a card from the reserve on to the empty tableau slot"))
	    ((not (empty-slot? 1))
	     (list 1 (get-name (get-top-card 1)) "on to the empty tableau slot"))
	    (#t #f))
      #f))

(define (get-hint)
  (or (base-not-set?)
      (to-foundations 1)
      (to-tableau 1)
      (empty-tableau? 7)
      (dealable?)))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
