; AisleRiot - streets_and_alleys.scm
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

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot DECK)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-extended-slot '() right)
  (deal-cards-face-up 1 '(0 2 3 5 6 8 9 11 0 2 3 5 6 8 9 11 
			  0 2 3 5 6 8 9 11 0 2 3 5 6 8 9 11 
			  0 2 3 5 6 8 9 11 0 2 3 5 6 8 9 11 0 3 6 9))


  (list 9 4))

(define (button-pressed slot-id card-list)
  (= (length card-list) 1))

(define (button-released start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((or (= end-slot 1)
	     (= end-slot 4)
	     (= end-slot 7)
	     (= end-slot 10))
	 (cond ((empty-slot? end-slot)
		(and (= (get-value (car card-list)) 1)
		     (move-n-cards! start-slot end-slot card-list)
		     (add-to-score! 1)))
	       ((and (= (get-suit (get-top-card end-slot))
			(get-suit (car card-list)))
		     (= (+ 1 (get-value (get-top-card end-slot)))
			(get-value (car card-list))))
		(begin
		  (move-n-cards! start-slot end-slot card-list)
		  (add-to-score! 1)))
	       (#t #f)))
	((empty-slot? end-slot)
	 (begin
	   (move-n-cards! start-slot end-slot card-list)
	   (if (or (= start-slot 1)
		   (= start-slot 4)
		   (= start-slot 7)
		   (= start-slot 10))
	       (add-to-score! -1))))
	((= (get-value (get-top-card end-slot))
	    (+ 1 (get-value (car card-list))))
	 (begin
	   (move-n-cards! start-slot end-slot card-list)
	   (if (or (= start-slot 1)
		   (= start-slot 4)
		   (= start-slot 7)
		   (= start-slot 10))
	       (add-to-score! -1))))
	(#t #f)))
	
(define (button-clicked slot-id) 
  #f)

(define (button-double-clicked slot-id)
  (if (and (not (empty-slot? slot-id))
	   (is-visible? (get-top-card slot-id))
	   (not (or (= slot-id 1)
		    (= slot-id 4)
		    (= slot-id 7)
		    (= slot-id 10))))
      (cond ((= (get-value (get-top-card slot-id)) ace)
	     (and (or (and (empty-slot? 1)
			   (deal-cards slot-id '(1)))
		      (and (empty-slot? 4)
			   (deal-cards slot-id '(4)))
		      (and (empty-slot? 7)
			   (deal-cards slot-id '(7)))
		      (deal-cards slot-id '(1)))
		  (add-to-score! 1)))
	    ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 1))))
	     (and (= (get-value (get-top-card slot-id))
		     (+ 1 (get-value (get-top-card 1))))
		  (deal-cards slot-id '(1))
		  (add-to-score! 1)))
	    ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 4))))
	     (and (= (get-value (get-top-card slot-id))
		     (+ 1 (get-value (get-top-card 4))))
		  (deal-cards slot-id '(4))
		  (add-to-score! 1)))
	    ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 7))))
	     (and (= (get-value (get-top-card slot-id))
		     (+ 1 (get-value (get-top-card 7))))
		  (deal-cards slot-id '(7))
		  (add-to-score! 1)))
	    ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 10))))
	     (and (= (get-value (get-top-card slot-id))
		     (+ 1 (get-value (get-top-card 10))))
		  (deal-cards slot-id '(10))
		  (add-to-score! 1)))
	    (#t #f))
      #f))
	    
(define (game-continuable)
  (not (game-won)))

(define (game-won)
  (and (= (length (get-cards 1)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 7)) 13)
       (= (length (get-cards 10)) 13)))

(define (check-a-foundation slot1 slot2)
  (cond ((or (= slot1 1)
	     (= slot1 4)
	     (= slot1 7)
	     (= slot1 10))
	 #f)
	((empty-slot? slot2)
	 (and (= (get-value (get-top-card slot1)) ace)
	      (list 2 (get-name (get-top-card slot1)) "an empty Foundation")))
	((and (= (get-suit (get-top-card slot1))
		 (get-suit (get-top-card slot2)))
	      (= (get-value (get-top-card slot1))
		 (+ 1 (get-value (get-top-card slot2)))))
	 (list 1
	       (get-name (get-top-card slot1))
	       (get-name (get-top-card slot2))))
	(#t #f)))

(define (check-to-foundation slot-id)
  (if (not (empty-slot? slot-id))
      (or (check-a-foundation slot-id 1)
	  (check-a-foundation slot-id 4)
	  (check-a-foundation slot-id 7)
	  (check-a-foundation slot-id 10)
	  (and (< slot-id 11)
	       (check-to-foundation (+ 1 slot-id))))
      (if (< slot-id 11)
	  (check-to-foundation (+ 1 slot-id))
	  #f)))

(define (get-hint)
  (or (check-to-foundation 0)
      (list 0 "Try rearranging the cards")))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
