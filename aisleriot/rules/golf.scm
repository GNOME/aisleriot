; AisleRiot - golf.scm
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
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards-face-up 0 '(2 3 4 5 6 7 8 2 3 4 5 6 7 8 2 3 4 5 6 7 8 2
			    3 4 5 6 7 8 2 3 4 5 6 7 8 ))

  (give-status-message)

  (list 7 3))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 1)
       (= (length card-list) 1)))

(define (button-released start-slot card-list end-slot)
  (and (= end-slot 1)
       (not (empty-slot? 1))
       (not (= (get-value (get-top-card 1)) king))
       (or (= (get-value (car card-list))
	      (+ 1 (get-value (get-top-card 1))))
	   (= (+ 1 (get-value (car card-list)))
	      (get-value (get-top-card 1))))
       (move-n-cards! start-slot end-slot card-list)
       (add-to-score! 1)))

(define (button-clicked slot-id)
  (or (and (= slot-id 0)
	   (not (empty-slot? slot-id))
	   (deal-cards-face-up 0 '(1)))
      (and (> slot-id 1)
	   (not (empty-slot? slot-id))
	   (not (empty-slot? 1))
	   (not (= (get-value (get-top-card 1))
		   king))
	   (or (= (get-value (get-top-card slot-id))
		  (+ 1 (get-value (get-top-card 1))))
	       (= (+ 1 (get-value (get-top-card slot-id)))
		  (get-value (get-top-card 1))))
	   (deal-cards slot-id '(1))
	   (add-to-score! 1))))

(define (button-double-clicked slot-id)
(display "bdc\n")
  (button-clicked slot-id))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 2)
       (empty-slot? 3)
       (empty-slot? 4)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)))

(define (check-slots slot)
  (cond ((or (empty-slot? 1)
	     (= (get-value (get-top-card 1)) king)
	     (= slot 9))
	 #f)
	((and (not (empty-slot? slot))
	      (or (= (get-value (get-top-card slot))
		     (+ 1 (get-value (get-top-card 1))))
		  (= (+ 1 (get-value (get-top-card slot)))
		     (get-value (get-top-card 1)))))
	 (list 1 (get-name (get-top-card slot)) (get-name (get-top-card 1))))
	(#t (check-slots (+ 1 slot)))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 "Deal another card")))

(define (get-hint)
  (or (check-slots 2)
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
