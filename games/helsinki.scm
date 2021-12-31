; AisleRiot - helsinki.scm
; Copyright (C) 2001, 2003 Rosanna Yuen <zana@webwynk.net>
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
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (> slot-id 0)
       (not (empty-slot? slot-id))
       (= (length card-list) 1)
       (not (= (get-value (get-top-card slot-id)) king))))

(define (droppable? start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (> end-slot 0)
       (= 13 (+ (get-value (car card-list))
		(get-value (get-top-card end-slot))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
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
       ; This is slightly odd, but the return value is important
       (and (or (empty-slot? 0)
		(deal-cards-face-up 0 (list slot-id)))
	    #t)))

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

(define (hint-remove-king suit)
  (cond ((eq? suit club) (G_"Remove the king of clubs."))
        ((eq? suit diamond) (G_"Remove the king of diamonds."))
        ((eq? suit heart) (G_"Remove the king of hearts."))
        ((eq? suit spade) (G_"Remove the king of spades."))))

(define (check-for-moves slot1 slot2)
  (cond ((= slot1 11)
	 #f)
	((empty-slot? slot1)
	 (check-for-moves (+ 1 slot1) (+ 2 slot1)))
	((= (get-value (get-top-card slot1)) king)
	 (hint-click slot1 (hint-remove-king (get-suit (get-top-card slot1)))))
	((= slot2 11)
	 (check-for-moves (+ 1 slot1) (+ 2 slot1)))
	((and (not (empty-slot? slot2))
	      (= 13 (+ (get-value (get-top-card slot1))
		       (get-value (get-top-card slot2)))))
	 (hint-move slot1 1 slot2))
	(#t (check-for-moves slot1 (+ 1 slot2)))))

(define (get-hint)
  (check-for-moves 1 2))

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
