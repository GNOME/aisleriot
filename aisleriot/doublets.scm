; AisleRiot - doublets.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>
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

  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (deal-cards-face-up 0 '(2 3 4 5 6 7 8 9))

  (check-kings '(2 3 4 5 6 7 8 9))

  (add-to-score! 1)

  (give-status-message)

  (list 6 3)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (get-redeals-string)
  (string-append "Redeals left:  "
		 (number->string (- 2 FLIP-COUNTER))))

(define (check-kings slot-list)
  (if (= (get-value (get-top-card (car slot-list))) king)
      (begin
	(let ((new-deck (get-cards 0)))
	      (set-cards! 0 (reverse (cons (make-card (get-value (get-top-card (car slot-list)))
						      (get-suit (get-top-card (car slot-list))))
					   (reverse new-deck)))))
	(remove-card (car slot-list))
	(deal-cards-face-up 0 (cons (car slot-list) '()))
	(check-kings slot-list))
      (if (> (length slot-list) 1)
	  (check-kings (cdr slot-list)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (not (= slot-id 0))
       (not (= slot-id 8))
       (not (= (get-value (car card-list)) king))))

(define (button-released start-slot card-list end-slot)
  (if (= end-slot 8)
      (if (or (= (get-value (car card-list))
		 (* 2 (get-value (get-top-card end-slot))))
	      (= (get-value (car card-list))
		 (- (* 2 (get-value (get-top-card end-slot))) 13)))
	  (begin
	    (add-to-score! 1)
	    (move-n-cards! start-slot end-slot card-list)
	    (cond ((not (empty-slot? 1))
		   (deal-cards 1 (cons start-slot '())))
		  ((not (empty-slot? 0))
		   (deal-cards-face-up 0 (cons start-slot '())))
		  (#t #t)))
	  #f)
      #f))

(define (button-clicked slot-id)
  (if (= slot-id 0)
      (flip-stock 0 1 2)))

(define (button-double-clicked slot)
  (if (and (not (empty-slot? slot))
	   (not (= slot 0))
	   (not (= slot 8))
	   (not (= (get-value (get-top-card slot)) king))
	   (= (get-value (get-top-card slot))
	      (modulo (* 2 (get-value (get-top-card 8))) 13)))
      (begin
	(add-to-score! 1)
	(deal-cards slot '(8))
	(cond ((not (empty-slot? 1))
	       (deal-cards 1 (cons slot '())))
	      ((and (not (empty-slot? 0))
		    (not (= slot 1)))
	       (deal-cards-face-up 0 (cons slot '())))
	      (#t #t)))
      #f))

(define (check-move slot)
  (if (and (not (empty-slot? slot))
	   (= (get-value (get-top-card slot))
	      (modulo (* 2 (get-value (get-top-card 8))) 13)))
      #t
      (if (< slot 9)
	  (check-move (+ 1 slot))
	  #f)))

(define (game-over)
  (give-status-message)
  (and (not (game-won))
       (or (< FLIP-COUNTER 2)
	   (not (empty-slot? 0))
	   (check-move 1))))

(define (game-won)
  (= 48 (length (get-cards 8))))

(define (get-hint)
  (let ((wanted (modulo (* 2 (get-value (get-top-card 8))) 
			13)))
    (list 4 (get-value-name wanted))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
