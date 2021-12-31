; AisleRiot - valentine.scm
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
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (deal-cards-face-up 0 '(2 3 4 5))

  (list 4 2))

(define (button-pressed slot-id card-list)
  (and (> slot-id 0)
       (not (empty-slot? slot-id))))

(define (droppable? start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (not (= start-slot end-slot))
       (> end-slot 1)
       (= (get-suit (get-top-card end-slot))
	  (get-suit (car card-list)))
       (= (get-value (get-top-card end-slot))
	  (+ 1 (get-value (car card-list))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)))

(define (flip-each-card card-list)
  (if (= (length card-list) 1)
      (list (flip-card (car card-list)))
      (cons (flip-card (car card-list)) (flip-each-card (cdr card-list)))))

(define (make-all-cards-invisible slot-id)
  (if (< slot-id 6)
      (begin	
	(set-cards! slot-id (flip-each-card (get-cards slot-id)))
	(make-all-cards-invisible (+ 1 slot-id)))
      #t))

(define (button-clicked slot-id)
  (cond ((or (> slot-id 0)
	     (and (empty-slot? 0)
		  (empty-slot? 1)))
	 #f)
	((or (empty-slot? 2)
	     (empty-slot? 3)
	     (empty-slot? 4)
	     (empty-slot? 5))
	 (and (or (not (empty-slot? 2))
		  (and (not (empty-slot? 1))
		       (deal-cards 1 '(2)))
		  (deal-cards-face-up 0 '(2)))
	      (or (not (empty-slot? 3))
		  (and (not (empty-slot? 1))
		       (deal-cards 1 '(3)))
		  (deal-cards-face-up 0 '(3)))
	      (or (not (empty-slot? 4))
		  (and (not (empty-slot? 1))
		       (deal-cards 1 '(4)))
		  (deal-cards-face-up 0 '(4)))
	      (or (not (empty-slot? 5))
		  (and (not (empty-slot? 1))
		       (deal-cards 1 '(5)))
		  (deal-cards-face-up 0 '(5)))))
	((empty-slot? 1)
	 (deal-cards-face-up 0 '(1)))
	(#t
	 (and (make-all-cards-invisible 2)	  
	      (set-cards! 0 (append (get-cards slot-id) 
				    (reverse (get-cards 5))
				    (reverse (get-cards 4))
				    (reverse (get-cards 3))
				    (reverse (get-cards 2))))
	      (set-cards! 2 '())
	      (set-cards! 3 '())
	      (set-cards! 4 '())
	      (set-cards! 5 '())	      
	      (deal-cards 1 '(2))
	      (deal-cards-face-up 0 '(3 4 5))))))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (not (game-won)))

(define (game-won)
  (and (empty-slot? 1)
       (empty-slot? 0)))

(define (tableau-move? slot1 slot2)
  (cond ((= slot1 6)
	 #f)
	((or (= slot2 6)
	     (empty-slot? slot1))
	 (tableau-move? (+ 1 slot1) 2))
	((and (not (empty-slot? slot2))
	      (= (get-suit (get-top-card slot1))
		 (get-suit (get-top-card slot2)))
	      (= (+ 1 (get-value (get-top-card slot1)))
		 (get-value (get-top-card slot2))))
	 (hint-move slot1 1 slot2))
	(#t (tableau-move? slot1 (+ 1 slot2)))))


(define (get-hint)
  (or (tableau-move? 1 2)
      (list 0 (G_"Deal more cards"))))



(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature scores-disabled)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable?)
