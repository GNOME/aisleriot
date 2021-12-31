; AisleRiot - scorpion.scm
; Copyright (C) 1999 Rosanna Yuen <rwsy@mit.edu>
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
; winning game seed: 2036201447

(define tableau '(1 2 3 4 5 6 7))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (add-blank-slot)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards 0 '(1 2 3 4))
  (deal-cards-face-up 0 '(5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7))

  (check-score)

  (list 9 4))

(define (check-score-cards acc cards unbroken count)
  (cond
      ((null? cards)
       (if (and unbroken (= count 13))
           (+ acc 4)
           acc))
      ((not (is-visible? (car cards)))
       (check-score-cards (- acc 3) (cdr cards) #f (+ count 1)))
      ((or (null? (cdr cards))
           (not (is-visible? (cadr cards))))
       (check-score-cards acc (cdr cards) unbroken (+ count 1)))
      ((and (= (get-suit (car cards))
               (get-suit (cadr cards)))
            (= (+ 1 (get-value (car cards)))
               (get-value (cadr cards))))
       (check-score-cards (+ acc 1) (cdr cards) unbroken (+ count 1)))
      (#t
       (check-score-cards acc (cdr cards) #f (+ count 1)))))

(define (check-score-slot acc slots)
  (if (null? slots)
      acc
      (check-score-slot (check-score-cards acc (get-cards (car slots)) #t 0) (cdr slots))))

(define (check-score)
  (set-score! (check-score-slot 36 tableau)))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (is-visible? (car (reverse card-list)))))

(define (correct-sequence card-list)
  (or (= (length card-list) 1)
      (and (is-visible? (cadr card-list))
	   (eq? (get-suit (car card-list))
		(get-suit (cadr card-list)))
	   (= (+ 1 (get-value (car card-list)))
	      (get-value (cadr card-list)))
	   (correct-sequence (cdr card-list)))))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (not (= end-slot 0))
       (or (and (empty-slot? end-slot)
		(= (get-value (car (reverse card-list))) king))
	   (and (not (empty-slot? end-slot))
		(eq? (get-suit (get-top-card end-slot))
		     (get-suit (car (reverse card-list))))
		(= (get-value (get-top-card end-slot))
		   (+ 1 (get-value (car (reverse card-list)))))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (or (empty-slot? start-slot)
	   (make-visible-top-card start-slot))
       (check-score)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (deal-cards-face-up 0 '(1 2 3))
       (check-score)))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (get-hint))

(define (slots-filled? slots)
  (cond
      ((null? slots)
       #t)
      ((empty-slot? (car slots))
       (slots-filled? (cdr slots)))
      ((and (= 13 (length (get-cards (car slots))))
            (correct-sequence (get-cards (car slots))))
       (slots-filled? (cdr slots)))
      (#t
       #f)))

(define (game-won)
  (slots-filled? tableau))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 (G_"Deal the cards"))))

(define (check-slot-cards slot1 slot2 count card card-list)
  (cond ((or (= (length card-list) 0)
	     (not (is-visible? (car card-list))))
	 #f)
	((and (eq? (get-suit card)
		   (get-suit (car card-list)))
	      (= (get-value card)
		 (+ 1 (get-value (car card-list)))))
	 (hint-move slot2 count slot1))
	(#t (check-slot-cards slot1 slot2 (+ count 1) card (cdr card-list)))))

(define (check-a-slot slot1 slot2)
  (cond ((= slot2 8)
	 #f)
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (check-slot-cards slot1 slot2 1 (get-top-card slot1) (get-cards slot2)))
	 (check-slot-cards slot1 slot2 1 (get-top-card slot1) (get-cards slot2)))
	(#t (check-a-slot slot1 (+ 1 slot2)))))

(define (check-slot slot-id)
  (cond ((= slot-id 8)
	 #f)
	((and (not (empty-slot? slot-id))
	      (check-a-slot slot-id 1))
	 (check-a-slot slot-id 1))
	(#t (check-slot (+ 1 slot-id)))))

(define (here-kingy-kingy slot-id count card-list)
  (cond ((or (= (length card-list) 0)
	     (= (length card-list) 1)
	     (not (is-visible? (car card-list))))
	 #f)
	((= (get-value (car card-list)) king)
	 (hint-move slot-id count (find-empty-slot tableau)))
	(#t (here-kingy-kingy slot-id (+ count 1) (cdr card-list)))))

(define (king-avail? slot-id)
  (cond ((= slot-id 8)
	 #f)
	((and (not (empty-slot? slot-id))
	      (here-kingy-kingy slot-id 1 (get-cards slot-id)))
	 (here-kingy-kingy slot-id 1 (get-cards slot-id)))
	(#t (king-avail? (+ 1 slot-id)))))

(define (check-for-empty)
  (and (or (empty-slot? 1)
	   (empty-slot? 2)
	   (empty-slot? 3)
	   (empty-slot? 4)
	   (empty-slot? 5)
	   (empty-slot? 6)
	   (empty-slot? 7))
       (king-avail? 1)))

(define (get-hint)
  (or (check-slot 1)
      (check-for-empty)
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
