; AisleRiot - auld_lang_syne.scm
; Copyright (C) 1999, 2003 Rosanna Yuen <rwsy@mit.edu>
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
  (set! DECK (make-deck-list-ace-low 2 2 club))
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-blank-slot)
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
  
  (add-card! 1 (make-visible (make-card ace club)))
  (add-card! 2 (make-visible (make-card ace diamond)))
  (add-card! 3 (make-visible (make-card ace heart)))
  (add-card! 4 (make-visible (make-card ace spade)))

  (give-status-message)

  (list 6 2)
)

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 4)))

(define (droppable? start-slot card-list end-slot)
  (and (< end-slot 5)
       (> end-slot 0)
       (= (get-value (car card-list))
          (+ 1 (get-value (get-top-card end-slot))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (add-to-score! 1)))

(define (dealable?)
  (not (empty-slot? 0)))

(define (do-deal-next-cards)
  (deal-cards-face-up 0 '(5 6 7 8)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (deal-cards-face-up 0 '(5 6 7 8))))

(define (check-end-slot? slot1 slot2)
  (if (and (not (empty-slot? slot1))
	   (= (get-value (get-top-card slot1))
	      (+ 1 (get-value (get-top-card slot2)))))
      (begin
	(deal-cards slot1 (list slot2))
	(add-to-score! 1))
      (if (< slot2 4)
	  (check-end-slot? slot1 (+ 1 slot2))
	  #f)))

(define (button-double-clicked slot-id)
  (and (> slot-id 4)
       (check-end-slot? slot-id 1)))

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (give-status-message)
  (and (empty-slot? 0)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)))

(define (movable? slot1 slot2)
  (if (= slot1 9)
      #f
      (if (or (= slot2 5)
	      (empty-slot? slot1))
	  (movable? (+ 1 slot1) 1)
	  (if (= (get-value (get-top-card slot1))
		 (+ 1 (get-value (get-top-card slot2))))
	      (hint-move slot1 1 slot2)
	      (movable? slot1 (+ 1 slot2))))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 (G_"Deal another round"))))

(define (get-hint)
  (or (movable? 5 1)
      (dealable?)))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable? dealable?)
