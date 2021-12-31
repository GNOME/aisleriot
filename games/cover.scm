; AisleRiot - cover.scm
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

  (deal-cards-face-up 0 '(1 2 3 4))

  (list 4 2))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (not (= slot-id 0))))

(define (droppable? start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (not (= end-slot 0))
       (= (get-suit (get-top-card end-slot))
          (get-suit (car card-list)))))


(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (add-to-score! 2)
       (remove-card end-slot)
       (or (empty-slot? 0)
	   (deal-cards-face-up 0 (list start-slot)))
       (or (empty-slot? 0)
	   (deal-cards-face-up 0 (list end-slot)))))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (and (give-status-message)
       (not (game-won))
       (get-hint)))

(define (game-won)
  (empty-slot? 0))

(define (check-suits slot1 slot2)
  (cond ((= slot1 4)
	 #f)
	((= slot2 5)
	 (check-suits (+ 1 slot1) (+ 2 slot1)))
	((= (get-suit (get-top-card slot1))
	    (get-suit (get-top-card slot2)))
	 (hint-move slot1 1 slot2))
	(#t (check-suits slot1 (+ 1 slot2)))))

(define (get-hint)
  (check-suits 1 2))

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
