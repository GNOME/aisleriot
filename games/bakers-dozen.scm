; AisleRiot - bakers_dozen.scm
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

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot DECK 'foundation)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)

  (add-carriage-return-slot)

  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

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

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((< end-slot 4)
	 (cond ((and (= (get-value (car card-list))
			ace)
		     (empty-slot? end-slot))
		#t)
	       ((and (not (empty-slot? end-slot))
		     (= (get-suit (get-top-card end-slot))
			(get-suit (car card-list)))
		     (= (+ 1 (get-value (get-top-card end-slot)))
			(get-value (car card-list))))
		#t)
	       (#t #f)))
	((and (not (empty-slot? end-slot))
	      (= (get-value (get-top-card end-slot))
		 (+ 1 (get-value (car card-list)))))
	 #t)
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (or (> start-slot 3)
           (add-to-score! -1))
       (or (> end-slot 3)
           (add-to-score! 1))))

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
	((if (empty-slot? f-slot)
	     (= (get-value (get-top-card t-slot)) ace)
	     (and (= (get-suit (get-top-card t-slot))
		     (get-suit (get-top-card f-slot)))
	          (= (get-value (get-top-card t-slot))
		     (+ 1 (get-value (get-top-card f-slot))))))
	 (hint-move t-slot 1 f-slot))
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
	 (hint-move slot1 1 slot2))
	(#t (tableau-moves? slot1 (+ 1 slot2)))))

(define (get-hint)
  (or (foundation-possible? 4 0)
      (tableau-moves? 4 5)
      (list 0 (G_"Try rearranging the cards"))))

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
