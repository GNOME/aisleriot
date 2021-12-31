; AisleRiot - thumb_and_pouch.scm
; Copyright (C) 1999, 2003 Rosanna Yuen <rwsy@mit.edu>
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

(define tableau '(6 7 8 9 10 11 12))
(define foundation '(2 3 4 5))

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

  (deal-cards 0 '(6 7 8 9 10 11 12 7 8 9 10 11 12 8 9 10 11 12 9 10 11 12 10 11 12 11 12 12))

  (map flip-top-card tableau)

  (give-status-message)

  (list 7 3))

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

(define (button-pressed slot-id card-list)
  (and (or (> slot-id 1)
	   (and (= slot-id 1)
		(= (length card-list) 1)))
       (is-visible? (car (reverse card-list)))))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (member start-slot foundation)
      (add-to-score! -1))
  (if (member end-slot foundation)
      (add-to-score! 1))
  (if (and (not (empty-slot? start-slot)) 
	   (member start-slot tableau))
      (make-visible-top-card start-slot))
  #t)

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (member end-slot tableau)
		(or (empty-slot? end-slot)
		    (and (not (eq? (get-suit (get-top-card end-slot))
				   (get-suit (car (reverse card-list)))))
			 (= (get-value (get-top-card end-slot))
			    (+ 1 (get-value (car (reverse card-list))))))))
	   (and (member end-slot foundation)
		(= 1 (length card-list))
		(or (and (empty-slot? end-slot)
			 (= ace (get-value (car card-list))))
		    (and (not (empty-slot? end-slot))
			 (eq? (get-suit (get-top-card end-slot))
			      (get-suit (car card-list)))
			 (= (get-value (get-top-card end-slot))
			    (- (get-value (car card-list)) 1))))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (flip-stock 0 1 2)
       (give-status-message)))

(define (button-double-clicked slot-id)
  (and (or (member slot-id tableau)
	   (= slot-id 1))
       (not (empty-slot? slot-id))
       (let* ((card (get-top-card slot-id))
	      (suit (get-suit card))
	      (value (get-value card)))
	 (let ((end-slot 
		(cond ((if (empty-slot? 2)
			   (= ace value)
			   (= suit (get-suit (get-top-card 2)))) 2)
		      ((if (empty-slot? 3)
			   (= ace value)
			   (= suit (get-suit (get-top-card 3)))) 3)
		      ((if (empty-slot? 4)
			   (= ace value)
			   (= suit (get-suit (get-top-card 4)))) 4)
		      ((if (empty-slot? 5)
			   (= ace value)
			   (= suit (get-suit (get-top-card 5)))) 5)
		      (#t #f))))
	   (and end-slot
		(or (= ace value)
		    (= (get-value (get-top-card end-slot)) (- value 1)))
		(remove-card slot-id)
		(complete-transaction slot-id (list card) end-slot))))))

(define (do-deal-next-cards)
  (flip-stock 0 1 2)
  (give-status-message))

(define (dealable?)
  (flippable? 0 1 2))

(define (game-continuable)
  (get-hint))

(define (game-won)
  (and (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))
       (= 13 (length (get-cards 5)))))

(define (empty-exist? slot-id)
  (cond ((= slot-id 13)
	 #f)
	((empty-slot? slot-id)
	 #t)
	(#t
	 (empty-exist? (+ 1 slot-id)))))

(define (check-waste-to-empty)
  (and (not (empty-slot? 1))
       (hint-move 1 1 (find-empty-slot tableau))))

(define (stripped-len card-list acc)
  (if (not (is-visible? (cadr card-list)))
      acc
      (stripped-len (cdr card-list) (+ 1 acc))))

(define (check-tableau-to-empty slot-id)
  (cond ((= slot-id 13)
	 #f)
	((and (not (empty-slot? slot-id))
	      (not (is-visible? (car (reverse (get-cards slot-id))))))
	 (hint-move slot-id (stripped-len (get-cards slot-id) 1) (find-empty-slot tableau)))
	(#t (check-tableau-to-empty (+ 1 slot-id)))))


(define (check-empty?)
  (and (empty-exist? 6)
       (or (check-tableau-to-empty 6)
	   (check-waste-to-empty))))

(define (check-a-foundation slot-id card f-slot)
  (cond ((or (> f-slot 5)
	     (or (< slot-id 0)
		 (empty-slot? slot-id)))
	 #f)
	((and (empty-slot? f-slot)
	      (= (get-value card) ace))
	 (if (< slot-id 0)
	     #t
	     (hint-move slot-id 1 f-slot)))
	((and (not (empty-slot? f-slot))
	      (eq? (get-suit (get-top-card f-slot))
		   (get-suit card))
	      (= (get-value card)
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (if (< slot-id 0)
	     #t
	     (hint-move slot-id 1 f-slot)))
	(#t
	 (check-a-foundation slot-id card (+ 1 f-slot)))))

(define (check-to-foundations slot-id)
  (cond ((> slot-id 12)
	 #f)
	((= slot-id 2)
	 (check-to-foundations 6))
	(#t
	 (or (check-a-foundation slot-id (get-top-card slot-id) 2)
	     (check-to-foundations (+ 1 slot-id))))))

(define (check-a-tslot from-slot to-slot num-cards card card-list)
  (and (not (or (= (length card-list) 0)
		(not (is-visible? (car card-list)))
		(>= (get-value (car card-list)) (get-value card))))
       (or (and (= (get-value card)
		   (+ 1 (get-value (car card-list))))
		(not (eq? (get-suit card)
			  (get-suit (car card-list))))
		(or (= (length card-list) 1)
		    (not (is-visible? (cadr card-list)))
		    (check-a-foundation -1 (cadr card-list) 2))
		(hint-move from-slot num-cards to-slot))
	   (check-a-tslot from-slot to-slot (+ 1 num-cards) card (cdr card-list)))))

(define (check-tslot to-slot from-slot)
  (cond ((> from-slot 12)
	 #f)
	((= from-slot 2)
	 (check-tslot to-slot 6))
	((empty-slot? to-slot)
	 (check-tslot to-slot (+ 1 from-slot)))
	(#t 
	 (or (and (= from-slot 1)
		  (not (empty-slot? 1))
		  (check-a-tslot from-slot to-slot 1
		                 (get-top-card to-slot)
				 (list (get-top-card from-slot))))
	     (check-a-tslot from-slot to-slot 1
	                    (get-top-card to-slot)
			    (get-cards from-slot))
	     (check-tslot to-slot (+ 1 from-slot))))))

(define (check-to-tableau slot-id)
  (and (not (> slot-id 12))
       (or (check-tslot slot-id 1)
	   (check-to-tableau (+ 1 slot-id)))))

(define (get-hint)
  (or (check-to-foundations 1)
      (check-to-tableau 6)
      (check-empty?)
      (or (and (not (empty-slot? 0))
               (list 0 (G_"Deal another round")))
          (and (not (empty-slot? 1))
               (< FLIP-COUNTER 2)
               (list 0 (G_"Move waste back to stock"))))))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable? dealable?)
