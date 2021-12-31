; AisleRiot - westhaven.scm
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

(define stock 0)
(define waste 1)
(define foundation '(2 3 4 5))
(define tableau '(6 7 8 9 10 11 12 13 14 15))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)
  (add-normal-slot '() 'waste)

  (add-blank-slot)

  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
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

  (deal-cards 0 '(6 7 8 9 10 11 12 13 14 15))
  (deal-cards 0 '(6 7 8 9 10 11 12 13 14 15))
  (deal-cards-face-up 0 '(6 7 8 9 10 11 12 13 14 15))

  (give-status-message)

  (list 10 4)
)

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (next-card card-list number)
  (if (= number 0)
      (car card-list)
      (next-card (cdr card-list) (- number 1))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 1)
	   (> slot-id 5))
       (is-visible? (car (reverse card-list)))
       (or (= (length card-list) 1)
	   (= (length card-list)
	      (length (get-cards slot-id)))
	   (not (is-visible? (next-card (get-cards slot-id)
					(length card-list)))))))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot) #f)
        ((> end-slot 5)
	 (or (empty-slot? end-slot)
	     (and (not (eq? (is-red? (get-top-card end-slot))
			    (is-red? (car (reverse card-list)))))
		  (= (get-value (get-top-card end-slot))
		     (+ 1 (get-value (car (reverse card-list))))))))
	((and (> end-slot 1)
	      (= 1 (length card-list)))
	 (if (= (get-value (car card-list)) ace)
	     (empty-slot? end-slot)
	     (and (not (empty-slot? end-slot))
		  (= (get-suit (car card-list))
		     (get-suit (get-top-card end-slot)))
		  (= (+ 1 (get-value (get-top-card end-slot)))
		     (get-value (car card-list))))))
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (begin
	 (move-n-cards! start-slot end-slot card-list)
	 (and (< end-slot 6)
	      (> end-slot 1)
	      (add-to-score! 1))
	 (or (empty-slot? start-slot)
	     (make-visible-top-card start-slot)))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (deal-cards-face-up 0 '(1))))

(define (double-clicked-move start-slot end-slot card-list)
  (move-n-cards! start-slot end-slot card-list)
  (remove-card start-slot)
  (add-to-score! 1)
  (if (not (empty-slot? start-slot))
      (make-visible-top-card start-slot)
      #t))

(define (button-double-clicked slot-id)
  (if (and (not (empty-slot? slot-id))
	   (or (= slot-id 1)
	       (> slot-id 5)))
      (if (= (get-value (get-top-card slot-id)) ace)
	  (cond ((empty-slot? 2)
		 (double-clicked-move slot-id 
				      2 
				      (list (get-top-card slot-id))))
		((empty-slot? 3)
		 (double-clicked-move slot-id 
				      3
				      (list (get-top-card slot-id))))
		((empty-slot? 4)
		 (double-clicked-move slot-id 
				      4 
				      (list (get-top-card slot-id))))
		(#t
		 (double-clicked-move slot-id 
				      5 
				      (list (get-top-card slot-id)))))
	  (cond ((and (not (empty-slot? 2))
		      (= (get-suit (get-top-card slot-id))
			 (get-suit (get-top-card 2)))
		      (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 2)))))
		 (double-clicked-move slot-id 
				      2 
				      (list (get-top-card slot-id))))
		((and (not (empty-slot? 3))
		      (= (get-suit (get-top-card slot-id))
			 (get-suit (get-top-card 3)))
		      (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 3)))))
		 (double-clicked-move slot-id 
				      3
				      (list (get-top-card slot-id))))
		((and (not (empty-slot? 4))
		      (= (get-suit (get-top-card slot-id))
			 (get-suit (get-top-card 4)))
		      (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 4)))))
		 (double-clicked-move slot-id 
				      4
				      (list (get-top-card slot-id))))
		((and (not (empty-slot? 5))
		      (= (get-suit (get-top-card slot-id))
			 (get-suit (get-top-card 5)))
		      (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 5)))))
		 (double-clicked-move slot-id 
				      5
				      (list (get-top-card slot-id))))
                (#t #f)))
      #f))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)))

(define (dealable?)
  (if (not (empty-slot? 0))
      (list 0 (G_"Deal a card"))
      #f))

(define (to-foundations? slot-id)
  (cond ((> slot-id 15)
	 #f)
	((= slot-id 2)
	 (to-foundations? 6))
	((empty-slot? slot-id)
	 (to-foundations? (+ 1 slot-id)))
	((= (get-value (get-top-card slot-id)) ace)
	 (hint-move slot-id 1 (find-empty-slot foundation)))
	((and (not (empty-slot? 2))
	      (eq? (get-suit (get-top-card 2))
		   (get-suit (get-top-card slot-id)))
	      (= (+ 1 (get-value (get-top-card 2)))
		 (get-value (get-top-card slot-id))))
	 (hint-move slot-id 1 2))
	((and (not (empty-slot? 5))
	      (eq? (get-suit (get-top-card 5))
		   (get-suit (get-top-card slot-id)))
	      (= (+ 1 (get-value (get-top-card 5)))
		 (get-value (get-top-card slot-id))))
	 (hint-move slot-id 1 5))
	((and (not (empty-slot? 3))
	      (eq? (get-suit (get-top-card 3))
		   (get-suit (get-top-card slot-id)))
	      (= (+ 1 (get-value (get-top-card 3)))
		 (get-value (get-top-card slot-id))))
	 (hint-move slot-id 1 3))
	((and (not (empty-slot? 4))
	      (eq? (get-suit (get-top-card 4))
		   (get-suit (get-top-card slot-id)))
	      (= (+ 1 (get-value (get-top-card 4)))
		 (get-value (get-top-card slot-id))))
	 (hint-move slot-id 1 4))
	(#t
	 (to-foundations? (+ 1 slot-id)))))

(define (waste-to-tableau? end-slot)
  (if (or (> end-slot 15)
	  (empty-slot? 1))
      #f
      (if (and (not (empty-slot? end-slot))
	       (not (eq? (is-red? (get-top-card 1))
			 (is-red? (get-top-card end-slot))))
	       (= (+ 1 (get-value (get-top-card 1)))
		  (get-value (get-top-card end-slot))))
	  (hint-move 1 1 end-slot)
	  (waste-to-tableau? (+ 1 end-slot)))))

(define (strip-invisible card-list)
  (if (is-visible? (car card-list))
      (car card-list)
      (strip-invisible (cdr card-list))))

(define (get-available-bottom slot-id)
  (strip-invisible (reverse (get-cards slot-id))))

(define (check-move card slot-id)
  (and (not (eq? (is-red? card)
		 (is-red? (get-top-card slot-id))))
       (= (+ 1 (get-value card))
	  (get-value (get-top-card slot-id)))))

(define (tableau-to-tableau? slot1 slot2)
  (cond ((= slot1 16)
	 #f)
	((or (empty-slot? slot1)
	     (= slot2 16))
	 (tableau-to-tableau? (+ 1 slot1) 6))
	((or (empty-slot? slot2)
	     (= slot1 slot2))
	 (tableau-to-tableau? slot1 (+ 1 slot2)))
	((check-move (get-available-bottom slot1) slot2)
	 (hint-move slot1 (find-card slot1 (get-available-bottom slot1)) slot2))
	(#t
	 (tableau-to-tableau? slot1 (+ 1 slot2)))))

(define (check-for-empty slot-id)
  (cond ((> slot-id 15)
	 #f)
	((empty-slot? slot-id)
	 slot-id)
	(#t
	 (check-for-empty (+ 1 slot-id)))))

(define (check-invisible slot-id)
  (cond ((> slot-id 15)
	 #f)

	((and (not (empty-slot? slot-id))
	      (not (is-visible? (car (reverse (get-cards slot-id))))))
	 slot-id)
	(#t
	 (check-invisible (+ 1 slot-id)))))

(define (check-empty-slot)
  (if (not (check-for-empty 6))
      #f
      (cond ((check-invisible 6)
             (let ((from-slot (check-invisible 6)))
              (hint-move from-slot (find-card from-slot (get-available-bottom from-slot)) (find-empty-slot tableau))))
	    ((not (empty-slot? 1))
	     (hint-move 1 1 (find-empty-slot tableau)))
	    (#t #f))))

(define (get-hint)
  (or (to-foundations? 1)
      (waste-to-tableau? 6)
      (tableau-to-tableau? 6 7)
      (check-empty-slot)
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
