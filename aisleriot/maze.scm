; AisleRiot - maze.scm
; Copyright (C) 2000 Matthew Wilcox <willy@linuxcare.com>
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

(define BASE-VAL 0)

(define score 0)

(define (add-line)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
;  (add-normal-slot '())
;  (add-normal-slot '())
)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
;  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-line)
  (add-carriage-return-slot)
  (add-line)
  (add-carriage-return-slot)
  (add-line)
  (add-carriage-return-slot)
  (add-line)
  (add-carriage-return-slot)
  (add-line)

  (deal-cards-face-up 0 '(    1  2  3  4  5  6  7
			   9 10 11 12 13 14 15 16
			  18 19 20 21 22 23 24 25 26
			  27 28 29 30 31 32 33 34 35
			  36 37 38 39 40 41 42 43 44
			  45 46 47 48 49 50 51 52 53))
  (make-visible-top-card 0)
  (eliminate-kings 53)

  (give-status-message)

  (list 11 5)
  (list 9 6)
)

(define (eliminate-kings slot)
  (and (not (empty-slot? slot))
       (= (get-value (get-top-card slot)) king)
       (remove-card slot))
  (and (> slot 0)
       (eliminate-kings (- slot 1))))

(define (give-status-message)
  (set-statusbar-message (string-append "Score: " (number->string score)))
  #f)

(define (button-pressed slot-id card-list)
  #t)

(define (card-val card-list)
  (get-value (car card-list)))

(define (card-suit card-list)
  (get-suit (car card-list)))

(define (card-inferior-value slot)
  (cond ((empty-slot? (- slot 1)) -1)
        (#t (get-value (get-top-card (- slot 1))))))

(define (card-superior-value slot)
  (cond ((empty-slot? (+ slot 1)) -1)
        (#t (get-value (get-top-card (+ slot 1))))))

(define (card-inferior-suit slot)
  (cond ((= slot 0) -1)
        ((empty-slot? (- slot 1)) -1)
        (#t (get-suit (get-top-card (- slot 1))))))

(define (card-superior-suit slot)
  (cond ((= slot 53) -1)
        ((empty-slot? (+ slot 1)) -1)
        (#t (get-suit (get-top-card (+ slot 1))))))

(define (button-released start-slot card-list end-slot)
  (cond ((not (empty-slot? end-slot)) #f)
        ((or (and (= (card-val card-list) ace)
                  (or (= end-slot 0)
		      (= (card-inferior-value end-slot) queen)))
               (and (= (card-val card-list) queen)
	          (or (= end-slot 53)
		      (= (card-superior-value end-slot) ace)))
	     (and (= (card-inferior-suit end-slot) (card-suit card-list))
	          (= (card-inferior-value end-slot) (- (card-val card-list) 1)))
	     (and (= (card-superior-suit end-slot) (card-suit card-list))
	          (= (card-superior-value end-slot) (+ (card-val card-list) 1))))
	  (move-n-cards! start-slot end-slot card-list))
        (#t #f)))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (in-order slot card suit)
  (set! score slot)
  (give-status-message)
  (cond ((= slot 54) #t)
  	((empty-slot? slot) (in-order (+ slot 1) card suit))
  	(#t (in-order-helper slot card
  			(if (= card ace) (get-suit (get-top-card slot)) suit)))
  )
)

(define (in-order-helper slot card suit)
  (cond
	((not (= (get-value (get-top-card slot)) card)) #f)
	((not (= (get-suit (get-top-card slot)) suit)) #f)
	(#t (in-order (+ slot 1) (+ (remainder card 12) 1) suit))
  )
)

(define (game-won)
  (in-order 0 ace -1))

(define (game-over)
	(not (game-won))
)

(define (get-hint)
  (list 0 "Aim to place the suits in the order which fits the current layout most naturally."))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-over game-won get-hint get-options
apply-options timeout)
