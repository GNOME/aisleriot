; AisleRiot - easthaven.scm
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

  (deal-cards 0 '(5 6 7 8 9 10 11 5 6 7 8 9 10 11))

  (deal-cards-face-up 0 '(5 6 7 8 9 10 11 ))

  (give-status-message)

  (list 7 4))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (> slot-id 1)
       (not (empty-slot? slot-id))
       (or (= (length card-list) 1)
	   (and (check-straight-descending-list card-list)
		(is-visible? (car (reverse card-list)))
		(check-alternating-color-list card-list)))))

(define (button-released start-slot card-list end-slot)
  (cond ((> end-slot 4)
	 (and (or (and (empty-slot? end-slot)
		       (= (get-value (car (reverse card-list))) king))
		  (and (not (empty-slot? end-slot))
		       (eq? (is-red? (get-top-card end-slot))
			    (is-black? (car (reverse card-list))))
		       (= (get-value (get-top-card end-slot))
			  (+ 1 (get-value (car (reverse card-list)))))))
	      (move-n-cards! start-slot end-slot card-list)
	      (or (and (< start-slot 5)
		       (add-to-score! -1))
		  (empty-slot? start-slot)
		  (make-visible-top-card start-slot))))
	((> end-slot 0)
	 (and (= (length card-list) 1)
	      (or (and (empty-slot? end-slot)
		       (= (get-value (car card-list)) ace))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (car card-list))
			  (get-suit (get-top-card end-slot)))
		       (= (get-value (car card-list))
			  (+ 1 (get-value (get-top-card end-slot))))))
	      (move-n-cards! start-slot end-slot card-list)
	      (add-to-score! 1)
	      (or (and (< start-slot 5)
		       (add-to-score! -1))
		  (empty-slot? start-slot)
		  (make-visible-top-card start-slot))))
	(#t #f)))

(define (try-dealing slot)
  (or (= slot 12)
      (empty-slot? 0)
      (and (deal-cards-face-up 0 (list slot))
	   (try-dealing (+ 1 slot)))))

(define (stripped card-list)
  (if (= (length card-list) 1)
      (car card-list)
      (if (and (is-visible? (car (reverse card-list)))
	       (check-straight-descending-list card-list)
	       (check-alternating-color-list card-list))
	  (car (reverse card-list))
	  (stripped (reverse (cdr (reverse card-list)))))))

(define (kings-avail slot)
  (cond ((= slot 12)
	 #f)
	((and (not (empty-slot? slot))
	      (not (is-visible? (car (reverse (get-cards slot)))))
	      (= (get-value (stripped (get-cards slot))) king))
	 slot)
	(#t (kings-avail (+ 1 slot)))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (or (and (not (empty-slot? 5))
		(not (empty-slot? 6))
		(not (empty-slot? 7))
		(not (empty-slot? 8))
		(not (empty-slot? 9))
		(not (empty-slot? 10))
		(not (empty-slot? 11)))
	   (not (kings-avail 5))
	   (> (+ (length (get-cards 0))
		 (get-score)) 45))
       (try-dealing 5)))

(define (move-double-click slot f-slot)
  (cond ((= f-slot 5)
	 #f)
	((and (empty-slot? f-slot)
	      (= (get-value (get-top-card slot)) ace))
	 (and (deal-cards slot (list f-slot))
	      (add-to-score! 1)
	      (or (empty-slot? slot)
		  (make-visible-top-card slot))))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card f-slot))
		 (get-suit (get-top-card slot))))
	 (and (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot))))
	      (deal-cards slot (list f-slot))
	      (add-to-score! 1)
	      (or (empty-slot? slot)
		  (make-visible-top-card slot))))
	(#t (move-double-click slot (+ 1 f-slot)))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (> slot-id 4)
       (move-double-click slot-id 1)))

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

(define (to-foundations? slot f-slot)
  (cond ((= slot 12)
	 #f)
	((or (empty-slot? slot)
	     (= f-slot 5))
	 (to-foundations? (+ 1 slot) 1))
	((and (empty-slot? f-slot)
	      (= (get-value (get-top-card slot)) ace))
	 (list 2
	       (get-name (get-top-card slot))
	       "an empty Foundation"))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card f-slot))
		 (get-suit (get-top-card slot)))
	      (= (+ 1 (get-value (get-top-card f-slot)))
		 (get-value (get-top-card slot))))
	 (list 1
	       (get-name (get-top-card slot))
	       (get-name (get-top-card f-slot))))
	(#t (to-foundations? slot (+ 1 f-slot)))))

(define (check-a-tab-slot card slot2)
  (and (eq? (is-red? card)
	    (is-black? (get-top-card slot2)))
       (= (+ 1 (get-value card))
	  (get-value (get-top-card slot2)))))

(define (check-tableau slot1 slot2)
  (cond ((= slot1 12)
	 #f)
	((or (empty-slot? slot1)
	     (= slot2 12))
	 (check-tableau (+ 1 slot1) 5))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (check-a-tab-slot (stripped (get-cards slot1)) slot2))
	 (list 1
	       (get-name (stripped (get-cards slot1)))
	       (get-name (get-top-card slot2))))
	(#t (check-tableau slot1 (+ 1 slot2)))))

(define (fill-empties slot)
  (cond ((= slot 12)
	 #f)
	((empty-slot? slot)
	 (and (kings-avail 5)
	      (list 0 "Move a King on to the empty Tableau slot")))
	(#t (fill-empties (+ 1 slot)))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 "Deal more cards")))

(define (get-hint)
  (or (to-foundations? 5 1)
      (check-tableau 5 6)
      (fill-empties 5)
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
