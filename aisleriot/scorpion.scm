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

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (deal-cards-face-up 0 '(1 2 3))))


(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (not (game-won)))

(define (check-wonness slot-id)
  (or (= slot-id 8)
      (and (or (empty-slot? slot-id)
	       (and (= (length (get-cards slot-id)) 13)
		    correct-sequence (get-cards slot-id)))
	   (check-wonness (+ 1 slot-id)))))

(define (game-won)
  (check-wonness 1)
)

(define (get-hint)
  #f)

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
