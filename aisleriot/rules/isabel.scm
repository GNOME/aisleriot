; AisleRiot - isabel.scm
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
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())

  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (get-vert-start))

  (set! VERTPOS (+ VERTPOS 80))

  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  
  (add-carriage-return-slot)  
  (add-carriage-return-slot)
  (add-carriage-return-slot)  
  (add-carriage-return-slot)
  (add-carriage-return-slot)  
  (add-carriage-return-slot)

  (add-normal-slot DECK)

  (deal-cards 13 '(0 1 2 3 4 5 6 7 8 9 10 11 12 0 1 2 3 4 5 6 7 8 9 10
		     11 12 0 1 2 3 4 5 6 7 8 9 10 11 12))
  (deal-cards-face-up 13 '(0 1 2 3 4 5 6 7 8 9 10 11 12))

  (list 5 3))

(define (button-pressed slot-id card-list)
  (not (empty-slot? slot-id)))

(define (button-released start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (not (= start-slot end-slot))
       (= (get-value (car card-list))
	  (get-value (get-top-card end-slot)))
       (remove-card end-slot)
       (add-to-score! 2)
       (or (empty-slot? start-slot)
	   (flip-top-card start-slot))
       (or (empty-slot? end-slot)
	   (flip-top-card end-slot))))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (= (get-score) 52))

(define (check-for-pairs slot1 slot2)
  (cond ((= slot1 13)
	 #f)
	((or (= slot2 13)
	     (empty-slot? slot1))
	 (check-for-pairs (+ 1 slot1) (+ 2 slot1)))
	((and (not (empty-slot? slot2))
	      (= (get-value (get-top-card slot1))
		 (get-value (get-top-card slot2))))
	 (list 1 
	       (get-name (get-top-card slot1)) 
	       (get-name (get-top-card slot2))))
	(#t (check-for-pairs slot1 (+ 1 slot2)))))

(define (get-hint)
  (check-for-pairs 0 1))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
