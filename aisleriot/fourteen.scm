; AisleRiot - fourteen.scm
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

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (add-carriage-return-slot)
  (set! VERTPOS (inexact->exact (truncate (+ VERTPOS 
					     (* 3 (/ (get-card-height) 4))))))

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (set! HORIZPOS (+ HORIZPOS 3000))
  (add-normal-slot DECK)

  (deal-cards-face-up 12 '(0 1 2 3 4 5 6 7 8 9 10 11))
  (deal-cards-face-up 12 '(0 1 2 3 4 5 6 7 8 9 10 11))
  (deal-cards-face-up 12 '(0 1 2 3 4 5 6 7 8 9 10 11))
  (deal-cards-face-up 12 '(0 1 2 3 4 5 6 7 8 9 10 11))
  (deal-cards-face-up 12 '(0 1 2 3))

  (list 6 3)
)

(define (button-pressed slot-id card-list)
  (if (and (not (empty-slot? slot-id))
	   (= (length card-list) 1))
      #t
      #f))

(define (button-released start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (not (= start-slot end-slot))
       (= 14 (+ (get-value (get-top-card end-slot))
		(get-value (car card-list))))
       (add-to-score! 2)
       (remove-card end-slot)))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 2)
       (empty-slot? 3)
       (empty-slot? 4)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 9)
       (empty-slot? 10)
       (empty-slot? 11)))

(define (check-slot slot1 slot2)
  (if (empty-slot? slot1)
      (if (< slot1 10)
	  (check-slot (+ 1 slot1) (+ 2 slot1))
	  #f)
      (if (or (empty-slot? slot2)
	      (not (= 14 (+ (get-value (get-top-card slot1))
			    (get-value (get-top-card slot2))))))
	  (if (< slot2 11)
	      (check-slot slot1 (+ 1 slot2))
	      (if (< slot1 10)
		  (check-slot (+ 1 slot1) (+ 2 slot1))
		  #f))
	  (list 1 
		(get-name (get-top-card slot1)) 
		(get-name (get-top-card slot2))))))
		  


(define (get-hint)
  (check-slot 0 1))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
