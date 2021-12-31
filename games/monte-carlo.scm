; AisleRiot - monte_carlo.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
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
  (add-blank-slot)
  (add-normal-slot '()) 
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
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
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
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
			    19 20 21 22 23 24 25))

  (give-status-message)

  (list 7 5))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (> slot-id 0)
       (not (empty-slot? slot-id))))

(define (adjacent? slot1 slot2)
  (or (and (= slot1 (+ 6 slot2))
	   (not (= (modulo slot1 5) 1)))
      (= slot1 (+ 5 slot2))
      (and (= slot1 (+ 4 slot2))
	   (not (= (modulo slot1 5) 0)))
      (and (= slot1 (+ 1 slot2))
	   (not (= (modulo slot1 5) 1)))
      (and (= slot1 (- slot2 1))
	   (not (= (modulo slot1 5) 0)))
      (and (= slot1 (- slot2 4))
	   (not (= (modulo slot1 5) 1)))
      (= slot1 (- slot2 5))
      (and (= slot1 (- slot2 6))
	   (not (= (modulo slot1 5) 0)))))



(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (> end-slot 0)
       (not (empty-slot? end-slot))
       (adjacent? start-slot end-slot)
       (= (get-value (car card-list))
	  (get-value (get-top-card end-slot)))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (add-to-score! 2)
       (remove-card end-slot)))

(define (deal-the-empties slot acc)
  (if (or (= slot 26)
	  (empty-slot? 0))
      acc
      (and (deal-cards-face-up 0 (list slot))
	   (deal-the-empties (+ 1 slot) #t))))

(define (moving-over slot blanks acc)
  (cond ((= slot 26)
	 (deal-the-empties (- 26 blanks) acc))
	((empty-slot? slot)
	 (moving-over (+ 1 slot) (+ 1 blanks) acc))
	((> blanks 0)
	 (and (deal-cards slot (list (- slot blanks)))
	      (moving-over (+ 1 slot) blanks #t)))
	(#t (moving-over (+ 1 slot) blanks acc))))

(define (move-cards-up)
  (moving-over 1 0 #f))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (move-cards-up)))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
      (get-hint)))


(define (game-won)
  (= 52 (get-score)))

(define (horizontal-check slot-id)
  (cond ((and (not (= 0 (modulo slot-id 5)))
	      (not (empty-slot? (+ 1 slot-id)))
	      (not (empty-slot? slot-id))
	      (= (get-value (get-top-card slot-id))
		 (get-value (get-top-card (+ 1 slot-id)))))
	 (hint-move slot-id 1 (+ 1 slot-id)))
	((< slot-id 24)
	 (horizontal-check (+ 1 slot-id)))
	(#t #f)))

(define (vertical-check slot-id)
  (cond ((and (not (empty-slot? slot-id))
	      (not (empty-slot? (+ 5 slot-id)))
	      (= (get-value (get-top-card slot-id))
		 (get-value (get-top-card (+ 5 slot-id)))))
	 (hint-move slot-id 1 (+ 5 slot-id)))
	((< slot-id 20)
	 (vertical-check (+ 1 slot-id)))
	(#t #f)))	       

(define (backslash-check slot-id)
  (cond ((and (not (= 0 (modulo slot-id 5)))
	      (not (empty-slot? (+ 6 slot-id)))
	      (not (empty-slot? slot-id))
	      (= (get-value (get-top-card slot-id))
		 (get-value (get-top-card (+ 6 slot-id)))))
	 (hint-move slot-id 1 (+ 6 slot-id)))
	((< slot-id 19)
	 (backslash-check (+ 1 slot-id)))
	(#t #f)))

(define (slash-check slot-id)
  (cond ((and (not (= 1 (modulo slot-id 5)))
	      (not (empty-slot? (+ 4 slot-id)))
	      (not (empty-slot? slot-id))
	      (= (get-value (get-top-card slot-id))
		 (get-value (get-top-card (+ 4 slot-id)))))
	 (hint-move slot-id 1 (+ 4 slot-id)))
	((< slot-id 20)
	 (slash-check (+ 1 slot-id)))
	(#t #f)))

(define (empty? slot yesblank)
  (cond ((= slot 26)
	 (if (and yesblank
		  (not (empty-slot? 0)))
	     (list 0 (G_"Deal more cards"))
	     #f))
	((empty-slot? slot)
	 (empty? (+ 1 slot) #t))
	(yesblank
	 (list 0 (G_"Deal more cards")))
	(#t (empty? (+ 1 slot) yesblank))))

(define (get-hint)
  (or (horizontal-check 1)
      (vertical-check 1)
      (backslash-check 1)
      (slash-check 2)
      (empty? 1 #f)))

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
