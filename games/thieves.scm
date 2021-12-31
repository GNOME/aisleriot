; Aisleriot - Thieves
; Copyright (C) 2001 Robert Brady <rwb197@ecs.soton.ac.uk>
;                    Rosanna Yuen <zana@webwynk.net>
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
  (make-joker-deck)
  
  (shuffle-deck)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-carriage-return-slot)
  (add-carriage-return-slot)
  (add-normal-slot DECK)
  (add-normal-slot '())
  (deal-cards-face-up 7 '(0 1 2 3 4 5 6 0 1 2 3 4 5 6 0 1 2 3 4 5 6 0
			    1 2 3 4 5 6 0 1 2 3 4 5 6 8))

  (give-status-message)

  (list 7 3))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 7)))))

(define (values-match? c1 c2)
  (or (eq? (get-value c1) joker)
      (eq? (get-value c2) joker)
      (eq? (+ 1 (get-value c1)) (get-value c2))
      (eq? (get-value c1) (+ 1 (get-value c2)))))

(define (score-for card)
  (cond ((eq? card ace) 8)
	((eq? card 2) 6)
	((eq? card 3) 6)
	((eq? card 4) 4)
	((eq? card 5) 4)
	((eq? card 6) 2)
	((eq? card 7) 2)
	((eq? card 8) 2)
	((eq? card 9) 4)
	((eq? card 10) 4)
	((eq? card jack) 6)
	((eq? card queen) 6)
	((eq? card king) 8)
	(#t 0)))

(define (can-move-from where)
  (and (not (empty-slot? where))
       (not (empty-slot? 8))
       (values-match? (get-top-card where) (get-top-card 8))))

(define (move-possible)
  (or (can-move-from 0)
      (can-move-from 1)
      (can-move-from 2)
      (can-move-from 3)
      (can-move-from 4)
      (can-move-from 5)
      (can-move-from 6)))

(define (button-pressed slot-id card-list) 
  (and (< slot-id 7)
       (not (empty-slot? slot-id))
       (= (length card-list) 1)))

(define (button-released start-slot card-list end-slot) 
  (and (= end-slot 8)
       (values-match? (car card-list) (get-top-card 8))
       (add-to-score! (score-for (get-value (car card-list))))
       (move-n-cards! start-slot 8 card-list)))

(define (droppable? start-slot card-list end-slot)
  (and (= end-slot 8)
       (values-match? (car card-list) (get-top-card 8)) ) )


(define (button-clicked slot-id) 
  (if (eq? slot-id 7)
      (and (not (empty-slot? slot-id))
	   (deal-cards-face-up 7 '(8)))
      (and (< slot-id 7)
	   (not (empty-slot? slot-id))
	   (values-match? (get-top-card slot-id) (get-top-card 8))
	   (add-to-score! (score-for (get-value (get-top-card slot-id))))
	   (deal-cards slot-id '(8)))))

(define (button-double-clicked slot)
  (button-clicked slot))

(define (game-won) 
  (and (empty-slot? 0) 
       (empty-slot? 1) 
       (empty-slot? 2) 
       (empty-slot? 3) 
       (empty-slot? 4)
       (empty-slot? 5) 
       (empty-slot? 6)))

(define (game-over) 
  (give-status-message)
  (if (game-won) 
    #f
    (if (empty-slot? 7) 
	(move-possible)
	#t)))

(define (hint-move-from where)
  (if (or (empty-slot? where)
          (empty-slot? where)
	  (not (values-match? (get-top-card where) (get-top-card 8))))
      #f
      (hint-move where 1 8)))

(define (get-hint)
  (or (hint-move-from 0)
      (hint-move-from 1)
      (hint-move-from 2)
      (hint-move-from 3)
      (hint-move-from 4)
      (hint-move-from 5)
      (hint-move-from 6)
      (list 0 (G_"Deal a card from the deck"))))

(define (get-options) #f)
(define (apply-options options) #f)
(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked 
  button-double-clicked game-over game-won get-hint get-options apply-options
  timeout droppable?)
