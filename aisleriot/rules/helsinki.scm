; AisleRiot - helsinki.scm
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

  (add-carriage-return-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 8 9 10))

  (give-status-message)

  (list 5 3))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (> slot-id 0)
       (not (empty-slot? slot-id))
       (= (length card-list) 1)))

(define (button-released start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (> end-slot 0)
       (= 13 (+ (get-value (car card-list))
		(get-value (get-top-card end-slot))))
       (remove-card end-slot)
       (add-to-score! 2)
       (or (empty-slot? 0)
	   (deal-cards-face-up 0 (list start-slot)))
       (or (empty-slot? 0)
	   (deal-cards-face-up 0 (list end-slot)))))
		

(define (button-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)
       (= (get-value (get-top-card slot-id)) king)
       (remove-card slot-id)
       (add-to-score! 1)
       (or (empty-slot? 0)
	   (deal-cards-face-up 0 (list slot-id)))))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 1)
       (empty-slot? 2)
       (empty-slot? 3)
       (empty-slot? 4)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 9)
       (empty-slot? 10)))

(define (check-for-moves slot1 slot2)
  (cond ((= slot1 11)
	 #f)
	((or (empty-slot? slot1)
	     (= slot2 11))
	 (check-for-moves (+ 1 slot1) (+ 2 slot1)))
	((= (get-value (get-top-card slot1)) king)
	 (list 2
	       (get-name (get-top-card slot1))
	       "itself"))
	((and (not (empty-slot? slot2))
	      (= 13 (+ (get-value (get-top-card slot1))
		       (get-value (get-top-card slot2)))))
	 (list 1
	       (get-name (get-top-card slot1))
	       (get-name (get-top-card slot2))))
	(#t (check-for-moves slot1 (+ 1 slot2)))))

(define (get-hint)
  (check-for-moves 1 2))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
