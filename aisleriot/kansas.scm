; AisleRiot - kansas.scm
; Copyright (C) 1999 Rosanna Yuen <rwsy@mit.edu>
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

  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (/ (get-card-width) 2)))))

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-normal-slot '())

  (add-blank-slot)
  (add-blank-slot)

  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (/ (get-card-width) 2)))))

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards 0 '(6 6 6 6 6 6 6 6 6 6 6))
  (deal-cards-face-up 0 '(6 2 7 8 9))

  (set! BASE-VAL (get-value (get-top-card 2)))

  (add-to-score! 1)

  (give-status-message)

  (list 7 4))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-reserve-no-string)
					"   "
					(get-base-string))))

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

(define (button-pressed slot-id card-list)
  (and (not (= (length card-list) 0))
       (is-visible? (car card-list))
       (or (= slot-id 1)
	   (> slot-id 5))))

(define (button-released start-slot card-list end-slot)
  (cond ((and (> end-slot 1)
	      (< end-slot 6)
	      (= (length card-list) 1))
	 (and (or (and (empty-slot? end-slot)
		       (= BASE-VAL (get-value (car card-list))))
		  (and (not (empty-slot? end-slot))
		       (eq? (get-suit (get-top-card end-slot))
			    (get-suit (car card-list)))
		       (or (and (= (get-value (get-top-card end-slot)) 
				   king)
				(= (get-value (car card-list)) ace))
			   (= (+ 1 (get-value (get-top-card end-slot)))
			      (get-value (car card-list))))))
	      (add-to-score! 1)
	      (move-n-cards! start-slot end-slot card-list)
	      (or (= start-slot 1)
		  (and (= start-slot 6)
		       (not (empty-slot? 6))
		       (make-visible-top-card 6))
		  (not (empty-slot? start-slot))
		  (empty-slot? 6)
		  (and (deal-cards 6 (list start-slot))
		       (or (empty-slot? 6)
			   (make-visible-top-card 6))
		       (give-status-message)))))
	((and (> end-slot 5)
	      (empty-slot? end-slot)
	      (= start-slot 1))
	 (move-n-cards! start-slot end-slot card-list))
	((and (> end-slot 5)
	      (not (empty-slot? end-slot)))
	 (and (or (and (= (get-value (get-top-card end-slot)) ace)
		       (= (get-value (car (reverse card-list))) king))
		  (= (get-value (get-top-card end-slot))
		     (+ 1 (get-value (car (reverse card-list))))))
	      (move-n-cards! start-slot end-slot card-list)
	      (or (= start-slot 1)
		  (empty-slot? 6)
		  (and (= start-slot 6)
		       (make-visible-top-card 6))
		  (not (empty-slot? start-slot))
		  (and (deal-cards 6 (list start-slot))
		       (or (empty-slot? 6)
			   (make-visible-top-card 6))
		       (give-status-message)))))
	(#t #f)))
		       

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (flip-stock 0 1 0)
       (give-status-message)))

(define (place-base slot-id foundation-slot)
  (cond ((empty-slot? foundation-slot)
	 (and (move-n-cards! slot-id 
			     foundation-slot
			     (list (get-top-card slot-id)))
	      (remove-card slot-id)
	      (add-to-score! 1)
	      (or (= slot-id 1)
		  (empty-slot? 6)
		  (and (= slot-id 6)
		       (make-visible-top-card 6))
		  (not (empty-slot? slot-id))
		  (and (deal-cards 6 (list slot-id))
		       (or (empty-slot? 6)
			   (make-visible-top-card 6))))
	      (give-status-message)))
	(#t
	 (place-base slot-id (+ 1 foundation-slot)))))

(define (place-card slot-id foundation-slot)
  (cond ((> foundation-slot 5)
	 #f)
	((and (not (empty-slot? foundation-slot))
	      (eq? (get-suit (get-top-card slot-id))
		   (get-suit (get-top-card foundation-slot)))
	      (or (and (= (get-value (get-top-card slot-id)) ace)
		       (= (get-value (get-top-card foundation-slot)) king))
		  (= (get-value (get-top-card slot-id))
		     (+ 1 (get-value (get-top-card foundation-slot))))))
	 (and (move-n-cards! slot-id 
			     foundation-slot 
			     (list (get-top-card slot-id)))
	      (add-to-score! 1)
	      (remove-card slot-id)
	      (or (= slot-id 1)
		  (empty-slot? 6)
		  (and (= slot-id 6)
		       (make-visible-top-card 6))
		  (not (empty-slot? slot-id))
		  (and (deal-cards 6 (list slot-id))
		       (or (empty-slot? 6)
			   (make-visible-top-card 6))))
	      (give-status-message)))
	(#t
	 (place-card slot-id (+ 1 foundation-slot)))))

(define (button-double-clicked slot-id)
  (if (and (or (> slot-id 5) 
	       (eq? slot-id 1))
	   (not (empty-slot? slot-id)))
      (cond ((= (get-value (get-top-card slot-id)) BASE-VAL)
	     (place-base slot-id 2))
	    (#t
	     (place-card slot-id 2)))
      #f))

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 "Deal another card")))

(define (check-a-foundation card foundation-slot)
  (cond ((> foundation-slot 5)
	 #f)
	((and (not (empty-slot? foundation-slot))
	      (eq? (get-suit card)
		   (get-suit (get-top-card foundation-slot)))
	      (or (and (= (get-value card) ace)
		       (= (get-value (get-top-card foundation-slot)) king))
		  (= (get-value card)
		     (+ 1 (get-value (get-top-card foundation-slot))))))
	 #t)
	(#t
	 (check-a-foundation card (+ 1 foundation-slot)))))

(define (check-to-foundations slot-id)
  (cond ((> slot-id 9)
	 #f)
	((= slot-id 2)
	 (check-to-foundations 6))
	((empty-slot? slot-id)
	 (check-to-foundations (+ 1 slot-id)))
	((= (get-value (get-top-card slot-id)) BASE-VAL)
	 (list 2 (get-name (get-top-card slot-id)) "an empty Foundation"))
	((check-a-foundation (get-top-card slot-id) 2)
	 (list 1 
	       (get-name (get-top-card slot-id))
	       (get-name (make-card (if (= (get-value (get-top-card slot-id))
					   ace)
					king
					(- (get-value (get-top-card slot-id))
					   1))
				    (get-suit (get-top-card slot-id))))))
	(#t
	 (check-to-foundations (+ 1 slot-id)))))

(define (check-a-tableau-list card card-list)
  (cond ((= (length card-list) 0)
	 #f)
	((or (= (get-value card)
		(+ 1 (get-value (car card-list))))
	     (and (= (get-value card) ace)
		  (= (get-value (car card-list)) king)))
	 (and (or (check-a-foundation (cadr card-list) 2)
		  (eq? (get-suit card)
		       (get-suit (car card-list))))
	      (list 1 (get-name (car card-list)) (get-name card))))
	(#t
	 (check-a-tableau-list card (cdr card-list)))))

(define (check-a-tableau slot1 slot2)
  (cond ((> slot2 9)
	 #f)
	((= slot2 2)
	 (check-a-tableau slot1 6))
	((or (= slot2 1)
	     (= slot2 6))
	 (or (and (not (empty-slot? slot2))
		  (or
		   (= (get-value (get-top-card slot1))
		      (+ 1 (get-value (get-top-card slot2))))
		   (and (= (get-value (get-top-card slot1)) ace)
			(= (get-value (get-top-card slot2)) king)))
		  (list 1 
			(get-name (get-top-card slot2))
			(get-name (get-top-card slot1))))
	     (check-a-tableau slot1 (+ 1 slot2))))
	((and (not (empty-slot? slot2))
	      (or (and (= (get-value (get-top-card slot1)) ace)
		       (= (get-value (car (reverse (get-cards slot2)))) king))
		  (= (get-value (get-top-card slot1))
		     (+ 1 (get-value (car (reverse (get-cards slot2))))))))
	 (list 1 
	       (get-name (car (reverse (get-cards slot2))))
	       (get-name (get-top-card slot1))))
	((and (not (empty-slot? slot2))
	      (or (> (get-value (get-top-card slot1))
		     (get-value (get-top-card slot2)))
		  (and (>= (get-value (get-top-card slot1)) ace)
		       (< (get-value (get-top-card slot1)) BASE-VAL)
		       (<= (get-value (get-top-card slot2)) king)
		       (> (get-value (get-top-card slot2)) BASE-VAL)))
	      (or (<= (get-value (get-top-card slot1))
		      (get-value (car (reverse (get-cards slot2)))))
		  (and (<= (get-value (get-top-card slot1)) king)
		       (> (get-value (get-top-card slot1)) BASE-VAL)
		       (>= (get-value (car (reverse (get-cards slot2)))) ace)
		       (< (get-value (car (reverse (get-cards slot2)))) BASE-VAL)))
	      (check-a-tableau-list (get-top-card slot1)
				    (get-cards slot2)))
	 (check-a-tableau-list (get-top-card slot1)
			       (get-cards slot2)))
	(#t
	 (check-a-tableau slot1 (+ 1 slot2)))))

(define (check-tableau slot-id)
  (cond ((> slot-id 9)
	 #f)
	((and (not (empty-slot? slot-id))
	      (check-a-tableau slot-id 1))
	 (check-a-tableau slot-id 1))
	(#t
	 (check-tableau (+ 1 slot-id)))))

(define (empty-tableau?)
  (and (not (empty-slot? 1))
       (or (empty-slot? 7)
	   (empty-slot? 8)
	   (empty-slot? 9))
       (list 2 (get-name (get-top-card 1)) "an empty Tableau slot")))

(define (get-hint)
  (or (check-to-foundations 1)
      (check-tableau 7)
      (empty-tableau?)
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
