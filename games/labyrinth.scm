; AisleRiot - labyrinth.scm
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

(def-save-var first-row #f)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (set! DECK (make-deck-list-ace-low 2 2 club))
  (shuffle-deck)

  (add-normal-slot DECK 'stock)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)

  (add-carriage-return-slot)

  (set! VERTPOS (- VERTPOS (/ 2 3)))

  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  
  (add-card! 1 (make-visible (make-card ace club)))
  (add-card! 2 (make-visible (make-card ace diamond)))
  (add-card! 3 (make-visible (make-card ace heart)))
  (add-card! 4 (make-visible (make-card ace spade)))

  (deal-cards-face-up 0 '(5 6 7 8 9 10 11 12))
  (set! first-row #t)

  (give-status-message)

  (list 8 4))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 4)
       (= (length card-list) 1)))

(define (droppable? start-slot card-list end-slot)
  (and (< end-slot 5)
       (> end-slot 0)
       (= (get-suit (get-top-card end-slot))
	  (get-suit (car card-list)))
       (= (+ 1 (get-value (get-top-card end-slot)))
	  (get-value (car card-list)))))
 
(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (or (and (not first-row)
		(or (> start-slot 12)
		    (empty-slot? (+ start-slot 8))
		    (and (set-cards! start-slot
				     (list (car (reverse (get-cards (+ start-slot 8))))))
			 (set-cards! (+ start-slot 8)
				     (reverse (cdr (reverse (get-cards (+ start-slot 8)))))))))
	   (empty-slot? 0)
	   (deal-cards-face-up 0 (list start-slot)))
       (add-to-score! 1)))
 
(define (check-slot-and-deal slot)
  (cond ((or (empty-slot? 0)
             (= slot 21))
	 #t)
	((empty-slot? (- slot 8))
	 (and (deal-cards-face-up 0 (list (- slot 8)))
	      (check-slot-and-deal (+ 1 slot))))
	(#t (and (deal-cards-face-up 0 (list slot))
		 (check-slot-and-deal (+ 1 slot))))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (set! first-row #f)
       (check-slot-and-deal 13)))

(define (button-double-clicked slot-id)
  (and (> slot-id 4)
       (not (empty-slot? slot-id))
       (or (and (= (get-suit (get-top-card slot-id)) club)
		(= (get-value (get-top-card slot-id))
		   (+ 1 (get-value (get-top-card 1))))
		(deal-cards slot-id '(1))
		(add-to-score! 1))
	   (and (= (get-suit (get-top-card slot-id)) diamond)
		(= (get-value (get-top-card slot-id))
		   (+ 1 (get-value (get-top-card 2))))
		(deal-cards slot-id '(2))
		(add-to-score! 1))
	   (and (= (get-suit (get-top-card slot-id)) heart)
		(= (get-value (get-top-card slot-id))
		   (+ 1 (get-value (get-top-card 3))))
		(deal-cards slot-id '(3))
		(add-to-score! 1))
	   (and (= (get-suit (get-top-card slot-id)) spade)
		(= (get-value (get-top-card slot-id))
		   (+ 1 (get-value (get-top-card 4))))
		(deal-cards slot-id '(4))
		(add-to-score! 1)))
       (or (and first-row
		(not (empty-slot? 0))
		(deal-cards-face-up 0 (list slot-id)))
	   (> slot-id 12)
	   (empty-slot? (+ 8 slot-id))
	   (and (set-cards! slot-id
			    (list (car (reverse (get-cards (+ slot-id 8))))))
		(set-cards! (+ slot-id 8)
			    (reverse (cdr (reverse (get-cards (+ slot-id 8))))))))))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 1)) 13)
       (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)))

(define (check-slot slot)
  (cond ((= slot 21)
	 #f)
	((empty-slot? slot)
	 (check-slot (+ 1 slot)))
	((and (= (get-suit (get-top-card slot)) club)
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card 1)))))
	 (hint-move slot 1 1))
	((and (= (get-suit (get-top-card slot)) diamond)
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card 2)))))
	 (hint-move slot 1 2))
	((and (= (get-suit (get-top-card slot)) heart)
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card 3)))))
	 (hint-move slot 1 3))
	((and (= (get-suit (get-top-card slot)) spade)
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card 4)))))
	 (hint-move slot 1 4))
	(#t (check-slot (+ 1 slot)))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 (G_"Deal more cards"))))

(define (get-hint)
  (or (check-slot 5)
      (dealable?)))

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

