; AisleRiot - first_law.scm
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

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (give-status-message)

  (list 6 2))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  #f)

(define (release-move-off? start-slot card-list check-slot)
  (if (or (= start-slot check-slot)
	  (and (not (empty-slot? check-slot))
	       (= (get-value (car card-list))
		  (get-value (get-top-card check-slot)))))
      (or (> check-slot 4)
	  (release-move-off? start-slot card-list (+ 1 check-slot)))
      #f))

(define (no-more-left? slot1 slot2)
  (cond ((= slot1 slot2)
	 #t)
	((and (not (empty-slot? slot2))
	      (= (get-value (get-top-card slot1))
		 (get-value (get-top-card slot2))))
	 #f)
	(#t (no-more-left? slot1 (+ 1 slot2)))))

(define (button-released start-slot card-list end-slot)
#f)

(define (move-off?)
  (and (not (empty-slot? 1))
       (not (empty-slot? 2))
       (not (empty-slot? 3))
       (not (empty-slot? 4))       
       (= (get-value (get-top-card 1))
	  (get-value (get-top-card 2))
	  (get-value (get-top-card 3))
	  (get-value (get-top-card 4)))
       (remove-card 1)
       (remove-card 2)
       (remove-card 3)
       (remove-card 4)
       (add-to-score! 1)))

(define (move-left? slot1 slot2)
  (cond ((= slot1 slot2)
	 #f)
	((and (not (empty-slot? slot2))
	      (= (get-value (get-top-card slot1))
		 (get-value (get-top-card slot2))))
	 (and (add-card! slot2 (get-top-card slot1))
	      (remove-card slot1)))
	(#t (move-left? slot1 (+ 1 slot2)))))

(define (button-clicked slot-id)
  (or (and (= slot-id 0)
	   (or (and (not (empty-slot? 0))
		    (deal-cards-face-up 0 '(1 2 3 4))
		    (give-status-message))
	       (and (or (empty-slot? 4)
			(flip-deck 0 4))
		    (or (empty-slot? 3)
			(flip-deck 0 3))
		    (or (empty-slot? 2)
			(flip-deck 0 2))
		    (or (empty-slot? 1)
			(flip-deck 0 1))
		    (give-status-message))))
      (and (not (empty-slot? slot-id))
	   (or (move-off?)
	       (move-left? slot-id 1)))))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (not (game-won)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 2)
       (empty-slot? 3)
       (empty-slot? 4)))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 "Deal another round")))

(define (removable?)
  (and (not (empty-slot? 1))
       (not (empty-slot? 2))
       (not (empty-slot? 3))
       (not (empty-slot? 4))
       (= (get-value (get-top-card 1))
	  (get-value (get-top-card 2))
	  (get-value (get-top-card 3))
	  (get-value (get-top-card 4)))
       (list 0 (string-append "Remove the "
			      (cond ((= (get-value (get-top-card 1)) 1)
				     "aces")
				    ((= (get-value (get-top-card 1)) 11)
				     "jacks")
				    ((= (get-value (get-top-card 1)) 12)
				     "queens")
				    ((= (get-value (get-top-card 1)) 13)
				     "kings")
				    (#t
				     (string-append (number->string (get-value (get-top-card 1)))
						    "'s")))))))

(define (move-leftable? slot1 slot2)
  (cond ((= slot1 4)
	 #f)
	((= slot2 5)
	 (move-leftable? (+ 1 slot1) (+ 2 slot1)))
	((and (not (empty-slot? slot1))
	      (not (empty-slot? slot2))
	      (= (get-value (get-top-card slot1))
		 (get-value (get-top-card slot2))))
	 (list 1 (get-name (get-top-card slot2))
	       (get-name (get-top-card slot1))))
	(#t
	 (move-leftable? slot1 (+ 1 slot2)))))

(define (get-hint)
  (or (removable?)
      (move-leftable? 1 2)
      (dealable?)
      (list 0 "Return cards to Stock")))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
