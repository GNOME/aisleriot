; AisleRiot - scorpion.scm
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
; winning game seed: 2036201447

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (add-blank-slot)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))

  (begin-score (reverse (get-cards 1)))
  (begin-score (reverse (get-cards 2)))
  (begin-score (reverse (get-cards 3)))
  (begin-score (reverse (get-cards 4)))
  (begin-score (reverse (get-cards 5)))
  (begin-score (reverse (get-cards 6)))
  (begin-score (reverse (get-cards 7)))

  (list 9 4))

(define (begin-score card-list)
  (if (not (is-visible? (car card-list)))
      (begin-score (cdr card-list))
      (begin
	(if (and (= (get-suit (car card-list))
		    (get-suit (cadr card-list)))
		 (= (get-value (car card-list))
		    (+ (get-value (cadr card-list)) 1)))
	    (add-to-score! 1))
	(if (> (length card-list) 2)
	    (begin-score (cdr card-list))
	    #f))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (is-visible? (car (reverse card-list)))))

(define (correct-sequence card-list)
  (or (= (length card-list) 1)
      (and (eq? (get-suit (car card-list))
		(get-suit (cadr card-list)))
	   (= (+ 1 (get-value (car card-list)))
	      (get-value (cadr card-list)))
	   (correct-sequence (cdr card-list)))))

(define (button-released start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (not (= end-slot 0))
       (or (and (empty-slot? end-slot)
		(= (get-value (car (reverse card-list))) king))
	   (and (not (empty-slot? end-slot))
		(eq? (get-suit (get-top-card end-slot))
		     (get-suit (car (reverse card-list))))
		(= (get-value (get-top-card end-slot))
		   (+ 1 (get-value (car (reverse card-list)))))
		(add-to-score! 1)))
       (move-n-cards! start-slot end-slot card-list)
       (or (empty-slot? start-slot)
	   (is-visible? (get-top-card start-slot))
	   (and (make-visible-top-card start-slot)
		(add-to-score! 3)))
       (or (not (= (length (get-cards end-slot)) 13))
	   (not (correct-sequence (get-cards end-slot)))
	   (and (= (length card-list) 13)
		(empty-slot? start-slot))
	   (add-to-score! 4))
       (or (not (= (length (get-cards start-slot)) 13))
	   (not (correct-sequence (get-cards start-slot)))
	   (add-to-score! 4))))

(define (check-for-points slot-id)
  (if (> slot-id 3)
      (give-status-message)
      (begin
	(if (and (> (length (get-cards slot-id)) 1)
		 (eq? (get-suit (get-top-card slot-id))
		      (get-suit (cadr (get-cards slot-id))))
		 (= (+ 1 (get-value (get-top-card slot-id)))
		    (get-value  (cadr (get-cards slot-id)))))
	    (add-to-score! 1)
	    #t)
	(check-for-points (+ 1 slot-id)))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (deal-cards-face-up 0 '(1 2 3))
       (check-for-points 1)))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (get-hint))

(define (check-wonness slot-id)
  (or (= slot-id 8)
      (and (or (empty-slot? slot-id)
	       (and (= (length (get-cards slot-id)) 13)
		    (correct-sequence (get-cards slot-id))))
	   (check-wonness (+ 1 slot-id)))))

(define (game-won)
  (check-wonness 1))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 "Deal the cards")))

(define (check-slot-cards card card-list)
  (cond ((or (= (length card-list) 0)
	     (not (is-visible? (car card-list))))
	 #f)
	((and (eq? (get-suit card)
		   (get-suit (car card-list)))
	      (= (get-value card)
		 (+ 1 (get-value (car card-list)))))
	 #t)
	(#t (check-slot-cards card (cdr card-list)))))

(define (check-a-slot slot1 slot2)
  (cond ((= slot2 8)
	 #f)
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (check-slot-cards (get-top-card slot1) (get-cards slot2)))
	 #t)
	(#t (check-a-slot slot1 (+ 1 slot2)))))

(define (check-slot slot-id)
  (cond ((= slot-id 8)
	 #f)
	((and (not (empty-slot? slot-id))
	      (check-a-slot slot-id 1))
	 (list 1 
	       (get-name (make-card (- (get-value (get-top-card slot-id)) 1)
				    (get-suit (get-top-card slot-id))))
	       (get-name (get-top-card slot-id))))
	(#t (check-slot (+ 1 slot-id)))))

(define (here-kingy-kingy card-list)
  (cond ((or (= (length card-list) 0)
	     (= (length card-list) 1)
	     (not (is-visible? (car card-list))))
	 #f)
	((= (get-value (car card-list)) king)
	 (list 2 (get-name (car card-list)) "an empty slot"))
	(#t (here-kingy-kingy (cdr card-list)))))

(define (king-avail? slot-id)
  (cond ((= slot-id 8)
	 #f)
	((and (not (empty-slot? slot-id))
	      (here-kingy-kingy (get-cards slot-id)))
	 (here-kingy-kingy (get-cards slot-id)))
	(#t (king-avail? (+ 1 slot-id)))))

(define (check-for-empty)
  (and (or (empty-slot? 1)
	   (empty-slot? 2)
	   (empty-slot? 3)
	   (empty-slot? 4)
	   (empty-slot? 5)
	   (empty-slot? 6)
	   (empty-slot? 7))
       (king-avail? 1)))

(define (get-hint)
  (or (check-slot 1)
      (check-for-empty)
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
