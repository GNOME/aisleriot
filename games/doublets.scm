; AisleRiot - doublets.scm
; Copyright (C) 1998, 2003 Rosanna Yuen <rwsy@mit.edu>
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
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards 0)))))

(define (get-redeals-string)
  (string-append (G_"Redeals left:") " "
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

(define (droppable? start-slot card-list end-slot)
  (and (= end-slot 8)
       (= (get-value (car card-list))
	  (modulo (* 2 (get-value (get-top-card end-slot))) 13))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (begin
	 (add-to-score! 1)
	 (move-n-cards! start-slot end-slot card-list)
	 (cond ((not (empty-slot? 1))
		(deal-cards 1 (cons start-slot '())))
	       ((not (empty-slot? 0))
		(deal-cards-face-up 0 (cons start-slot '())))
	       (#t #t)))))

(define (dealable?)
  (flippable? 0 1 2))

(define (do-deal-next-cards)
   (flip-stock 0 1 2))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (do-deal-next-cards)))

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

(define (get-value-hint value)
  (cond ((eq? value ace) (G_"You are searching for an ace."))
        ((eq? value 2) (G_"You are searching for a two."))
        ((eq? value 3) (G_"You are searching for a three."))
        ((eq? value 4) (G_"You are searching for a four."))
        ((eq? value 5) (G_"You are searching for a five."))
        ((eq? value 6) (G_"You are searching for a six."))
        ((eq? value 7) (G_"You are searching for a seven."))
        ((eq? value 8) (G_"You are searching for an eight."))
        ((eq? value 9) (G_"You are searching for a nine."))
        ((eq? value 10) (G_"You are searching for a ten."))
        ((eq? value jack) (G_"You are searching for a jack."))
        ((eq? value queen) (G_"You are searching for a queen."))
        ((eq? value king) (G_"You are searching for a king."))
        (#t (G_"Unknown value"))))

(define (get-hint)
  (let ((wanted (modulo (* 2 (get-value (get-top-card 8))) 
			13)))
    (list 0 (get-value-hint wanted))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable? dealable?)
