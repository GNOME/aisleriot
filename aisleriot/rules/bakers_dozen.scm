; AisleRiot - bakers_dozen.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
;
; This game is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
; USA

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot DECK)
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards-face-up 0 '(4 5 6 7 8 9 10 11 12 13 14 15 16 4 5 6 7 8 9
			    10 11 12 13 14 15 16))
  (move-kings-back 4)

  (deal-cards-face-up 0 '(4 5 6 7 8 9 10 11 12 13 14 15 16))
  (move-kings-back 4)

  (deal-cards-face-up 0 '(4 5 6 7 8 9 10 11 12 13 14 15 16))
  (move-kings-back 4)

  (list 13 5))

(define (move-kings-back slot-id)
  (if (< slot-id 17)
      (begin (and (= (get-value (get-top-card slot-id)) king)
		  (set-cards! slot-id (append (cdr (get-cards slot-id)) 
					      (list (get-top-card slot-id)))))
	     (move-kings-back (+ 1 slot-id)))
      #t))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (= (length card-list) 1)))

(define (button-released start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((< end-slot 4)
	 (cond ((and (= (get-value (car card-list))
			ace)
		     (empty-slot? end-slot))
		(and (move-n-cards! start-slot end-slot card-list)
		     (or (< start-slot 4)
			 (add-to-score! 1))))
	       ((and (not (empty-slot? end-slot))
		     (= (get-suit (get-top-card end-slot))
			(get-suit (car card-list)))
		     (= (+ 1 (get-value (get-top-card end-slot)))
			(get-value (car card-list))))
		(and (move-n-cards! start-slot end-slot card-list)
		     (add-to-score! 1)))
	       (#t #f)))
	((and (not (empty-slot? end-slot))
	      (= (get-value (get-top-card end-slot))
		 (+ 1 (get-value (car card-list)))))
	 (and (move-n-cards! start-slot end-slot card-list)
	      (or (> start-slot 3)
		  (add-to-score! -1))))
	(#t #f)))

(define (button-clicked slot-id)
  #f)

(define (find-empty-foundation a-slot f-slot)
  (cond ((> f-slot 3)
	 #f)
	((empty-slot? f-slot)
	 (deal-cards a-slot (list f-slot)))
	(#t (find-empty-foundation a-slot (+ 1 f-slot)))))

(define (find-foundation a-slot f-slot)
  (cond ((> f-slot 3)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card a-slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card a-slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (and (deal-cards a-slot (list f-slot))
	      (add-to-score! 1)))
	(#t (find-foundation a-slot (+ 1 f-slot)))))


(define (button-double-clicked slot-id)
  (and (> slot-id 3)
       (not (empty-slot? slot-id))
       (or (and (= (get-value (get-top-card slot-id))
		   ace)
		(find-empty-foundation slot-id 0)
		(add-to-score! 1))
	   (find-foundation slot-id 0))))
       

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 0)) 13)
       (= (length (get-cards 1)) 13)
       (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)))

(define (foundation-possible? t-slot f-slot)
  (cond ((= t-slot 17)
	 #f)
	((or (= f-slot 4)
	     (empty-slot? t-slot))
	 (foundation-possible? (+ 1 t-slot) 0))
	((= (get-value (get-top-card t-slot)) ace)
	 (list 2 (get-name (get-top-card t-slot)) "an empty foundation"))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card t-slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card t-slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (list 1 
	       (get-name (get-top-card t-slot)) 
	       (get-name (get-top-card f-slot))))
	(#t (foundation-possible? t-slot (+ 1 f-slot)))))

(define (card-to-foundation-possible? card f-slot)
  (cond ((= f-slot 4)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-suit card)
		 (get-suit (get-top-card f-slot))))
	 (= (get-value card)
	    (+ 1 (get-value (get-top-card f-slot)))))
	(#t (card-to-foundation-possible? card (+ 1 f-slot)))))

(define (tableau-moves? slot1 slot2)
  (cond ((= slot1 17)
	 #f)
	((or (= slot2 17)
	     (empty-slot? slot1))
	 (tableau-moves? (+ 1 slot1) 4))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (> (length (get-cards slot1)) 1)
	      (= (+ 1 (get-value (get-top-card slot1)))
		 (get-value (get-top-card slot2)))
	      (or (not (= (get-value (get-top-card slot2))
			  (get-value (cadr (get-cards slot1)))))
		  (card-to-foundation-possible? (cadr (get-cards slot1)) 0)))
	 (list 1 (get-name (get-top-card slot1)) (get-name (get-top-card slot2))))
	(#t (tableau-moves? slot1 (+ 1 slot2)))))

(define (get-hint)
  (or (foundation-possible? 4 0)
      (tableau-moves? 4 5)
      (list 0 "Try rearranging the cards")))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
