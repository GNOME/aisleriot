; AisleRiot - spiderette.scm
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

  (add-normal-slot DECK)

  (add-blank-slot)
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

  (deal-cards 0 '(5 6 7 8 9 10 11 6 7 8 9 10 11 7 8 9 10 11 8 9 10 11
		    9 10 11 10 11 11))
  (map flip-top-card '(5 6 7 8 9 10 11))

  (give-status-message)

  (list 7 4))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (> slot-id 4)
       (not (empty-slot? slot-id))
       (or (= (length card-list) 1)
	   (and (check-same-suit-list card-list)
		(check-straight-descending-list card-list)
		(is-visible? (car (reverse card-list)))))))

(define (button-released start-slot card-list end-slot)
  (cond ((> end-slot 4)
	 (and (or (empty-slot? end-slot)
		  (and (= (get-value (get-top-card end-slot))
			  (+ 1 (get-value (car (reverse card-list)))))
		       (or (not (= (get-suit (get-top-card end-slot))
				   (get-suit (car card-list))))
			   (add-to-score! 1))))
	      (or (empty-slot? start-slot)
		  (and (is-visible? (get-top-card start-slot))
		       (= (get-suit (get-top-card start-slot))
			  (get-suit (car card-list)))
		       (= (get-value (get-top-card start-slot))
			  (+ 1 (get-value (car card-list))))
		       (add-to-score! -1))
		  (make-visible-top-card start-slot))
	      (move-n-cards! start-slot end-slot card-list)))
	((> end-slot 0)
	 (and (empty-slot? end-slot)
	      (= (length card-list) 13)
	      (move-n-cards! start-slot end-slot (reverse card-list))))
	(#t #f)))

(define (try-dealing slot)
  (or (= slot 12)
      (empty-slot? 0)
      (and (deal-cards-face-up 0 (list slot))
	   (or (= (length (get-cards slot)) 1)
	       (not (is-visible? (cadr (get-cards slot))))
	       (and (= (get-suit (get-top-card slot))
		       (get-suit (cadr (get-cards slot))))
		    (= (+ 1 (get-value (get-top-card slot)))
		       (get-value (cadr (get-cards slot))))
		    (add-to-score! 1))
	       #t)
	   (try-dealing (+ 1 slot)))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 5))
       (not (empty-slot? 6))
       (not (empty-slot? 7))
       (not (empty-slot? 8))
       (not (empty-slot? 9))
       (not (empty-slot? 10))
       (not (empty-slot? 11))
       (try-dealing 5)))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 9)
       (empty-slot? 10)
       (empty-slot? 11)))

(define (check-a-tab-slot card slot2 same-suit?)
  (and (or (not same-suit?)
	   (= (get-suit card)
	      (get-suit (get-top-card slot2))))
       (= (+ 1 (get-value card))
	  (get-value (get-top-card slot2)))))

(define (stripped card-list slot)
  (if (= (length card-list) 1)
      (car card-list)
      (if (and (is-visible? (car (reverse card-list)))
	       (check-straight-descending-list card-list)
	       (check-same-suit-list card-list))
	  (car (reverse card-list))
	  (stripped (reverse (cdr (reverse card-list))) slot))))

(define (check-suit-moves slot1 slot2)
  (cond ((= slot1 12)
	 #f)
	((or (empty-slot? slot1)
	     (= slot2 12))
	 (check-suit-moves (+ 1 slot1) 5))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (check-a-tab-slot (stripped (get-cards slot1) slot1) slot2 #t))
	 (list 1 
	       (get-name (stripped (get-cards slot1) slot1))
	       (get-name (get-top-card slot2))))
	(#t (check-suit-moves slot1 (+ 1 slot2)))))

(define (check-not-suit-moves slot1 slot2)
  (cond ((= slot1 12)
	 #f)
	((or (empty-slot? slot1)
	     (= slot2 12))
	 (check-not-suit-moves (+ 1 slot1) 5))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (check-a-tab-slot (stripped (get-cards slot1) slot1) slot2 #f))
	 (list 1 
	       (get-name (stripped (get-cards slot1) slot1))
	       (get-name (get-top-card slot2))))
	(#t (check-not-suit-moves slot1 (+ 1 slot2)))))

(define (check-for-empties slot)
  (and (or (empty-slot? 5)
	   (empty-slot? 6)
	   (empty-slot? 7)
	   (empty-slot? 8)
	   (empty-slot? 9)
	   (empty-slot? 10)
	   (empty-slot? 11))
       (list 0 "Place something on to the empty Tableau slot")))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 "Deal more cards")))

(define (get-hint)
  (or (check-suit-moves 5 6)
      (check-not-suit-moves 5 6)
      (check-for-empties 5)
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
