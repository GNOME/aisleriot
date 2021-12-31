; AisleRiot - maze.scm
; Copyright (C) 2000 Matthew Wilcox <willy@linuxcare.com>
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

(define card '())

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

  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 9 10 11 12 13 14 15 16 18 19
			    20 21 22 23 24 25 26 27 28 29 30 31 32 33 
			    34 35 36 37 38 39 40 41 42 43 44 45 46 47 
			    48 49 50 51 52 53))

  (make-visible-top-card 0)
  (eliminate-kings 53)

  (calculate-score)

  (list 9 6))

(define (eliminate-kings slot)
  (and (not (empty-slot? slot))
       (= (get-value (get-top-card slot)) king)
       (remove-card slot))
  (and (> slot 0)
       (eliminate-kings (- slot 1))))

(define (button-pressed slot-id card-list)
  #t)

(define (suit-next? first second)
  (and (= (get-suit first)
	  (get-suit second))
       (= (+ 1 (get-value first))
	  (get-value second))))

(define (card-next? lower higher)
  (or (and (= ace (get-value higher))
	   (= queen (get-value lower)))
      (suit-next? lower higher)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (add-card! end-slot (car card-list))))

(define (droppable? start-slot card-list end-slot)
  (set! card (car card-list))
  (and (not (= start-slot end-slot))
       (empty-slot? end-slot)
       (or (if (= end-slot 0)
	       (= ace (get-value card))
	       (and (not (empty-slot? (- end-slot 1)))
		    (card-next? (get-top-card (1- end-slot)) card)))
	   (if (= end-slot 53)
	       (= queen (get-value card))
	       (and (not (empty-slot? (1+ end-slot)))
		    (card-next? card (get-top-card (1+ end-slot))))))))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (get-full-card slot)
  (if (empty-slot? slot)
      (get-full-card (1+ slot))
      (get-top-card slot)))

(define (calculate-score)
  (set! card (get-full-card 0))
  (if (= (get-value card) ace)
      (set-score! 1)
      (set-score! 0))
  (calculate-score-helper 1 card)
  (= (get-score) 48) ; 48 cards in the pack
)

(define (calculate-score-helper slot prev)
  (or (= slot 54)
      (and (empty-slot? slot)
	   (calculate-score-helper (1+ slot) prev))
      (and (set! card (get-top-card slot))
	   (card-next? prev card)
	   (add-to-score! 1)
	   #f)
      (calculate-score-helper (1+ slot) card)))

(define (game-won)
  (calculate-score))

(define (game-over)
  (not (game-won)))

(define (get-hint)
  (list 0 (G_"Aim to place the suits in the order which fits the current layout most naturally.")))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-over game-won get-hint get-options
apply-options timeout droppable?)
