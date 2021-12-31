; AisleRiot - quatorze.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
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
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
			    19 20 21 22 23 24 25))

  (give-status-message)

  (list 7 5))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)))

(define (shift-cards slot delta)
  (cond ((or (= slot 26)
	     (> (+ slot delta) 25))
	 #t)
	((empty-slot? (+ slot delta))
	 (shift-cards slot (+ 1 delta)))
	(#t
	 (and (deal-cards (+ slot delta) (list slot))
	      (shift-cards (+ 1 slot) delta)))))

(define (fill-in start-slot end-slot)
  (if (not (empty-slot? 0))
      (and (deal-cards-face-up 0 (list start-slot))
	   (or (and (not (empty-slot? 0))
		    (deal-cards-face-up 0 (list end-slot)))
	       (shift-cards end-slot 1)))
      (if (< start-slot end-slot)
	  (shift-cards start-slot 1)
	  (shift-cards end-slot 1))))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (not (empty-slot? end-slot))
       (> end-slot 0)
       (= 14 (+ (get-value (car card-list))
		(get-value (get-top-card end-slot))))
       (or (= (modulo start-slot 5)
	      (modulo end-slot 5))
	   (and (< start-slot 6)
		(< end-slot 6))
	   (and (> start-slot 5)
		(< start-slot 11)
		(> end-slot 5)
		(< end-slot 11))
	   (and (> start-slot 10)
		(< start-slot 16)
		(> end-slot 10)
		(< end-slot 16))
	   (and (> start-slot 15)
		(< start-slot 21)
		(> end-slot 15)
		(< end-slot 21))
	   (and (> start-slot 20)
		(> end-slot 20)))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (add-to-score! 2)
       (remove-card end-slot)
       (fill-in start-slot end-slot)))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (empty-slot? 1))

(define (check-row slot1 slot2 buffer)
  (cond ((= slot1 6)
	 #f)
	((or (empty-slot? (+ slot1 buffer))
	     (= slot2 6))
	 (check-row (+ 1 slot1) (+ 2 slot1) buffer))
	((and (not (empty-slot? (+ slot2 buffer)))
	      (= 14 (+ (get-value (get-top-card (+ slot1 buffer)))
		       (get-value (get-top-card (+ slot2 buffer))))))
	 (hint-move (+ slot1 buffer) 1 (+ slot2 buffer)))
	(#t (check-row slot1 (+ 1 slot2) buffer))))

(define (check-horiz)
  (or (check-row 1 2 0)
      (check-row 1 2 5)
      (check-row 1 2 10)
      (check-row 1 2 15)
      (check-row 1 2 20)))

(define (check-col slot1 slot2 buffer)
  (cond ((= slot1 26)
	 #f)
	((or (empty-slot? (+ slot1 buffer))
	     (= slot2 26))
	 (check-col (+ 5 slot1) (+ 10 slot1) buffer))
	((and (not (empty-slot? (+ slot2 buffer)))
	      (= 14 (+ (get-value (get-top-card (+ slot1 buffer)))
		       (get-value (get-top-card (+ slot2 buffer))))))
	 (hint-move (+ slot1 buffer) 1 (+ slot2 buffer)))
	(#t (check-col slot1 (+ 5 slot2) buffer))))

(define (check-vert)
  (or (check-col 1 6 0)
      (check-col 1 6 1)
      (check-col 1 6 2)
      (check-col 1 6 3)
      (check-col 1 6 4)))

(define (get-hint)
  (or (check-horiz)
      (check-vert)))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable?)
