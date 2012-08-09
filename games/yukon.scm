; AisleRiot - yukon.scm
; Copyright (C) 1998, 2003 Rosanna Yuen <rwsy@mit.edu>
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

(define foundation '(0 8 9 10))
(define tableau '(1 2 3 4 5 6 7))

(define (new-game)
  (initialize-playing-area)

					;set up the cards
  (make-standard-deck)
  (shuffle-deck)
  
					;set up the board
  (add-normal-slot DECK 'foundation)
  (add-blank-slot)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-carriage-return-slot)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-normal-slot '() 'foundation)


  (deal-cards 0 '(1 2 3 4 5 6 7 2 3 4 5 6 7 3 4 5 6 7 4 5 6 7 5 6 7 6 7 7))

  (flip-top-card 1)
  (flip-top-card 2)
  (flip-top-card 3)
  (flip-top-card 4)
  (flip-top-card 5)
  (flip-top-card 6)
  (flip-top-card 7)

  (deal-cards 0 '(2 3 4 5 6 7))
  (flip-top-card 2)
  (flip-top-card 3)
  (flip-top-card 4)
  (flip-top-card 5)
  (flip-top-card 6)
  (flip-top-card 7)
  (deal-cards 0 '(2 3 4 5 6 7))
  (flip-top-card 2)
  (flip-top-card 3)
  (flip-top-card 4)
  (flip-top-card 5)
  (flip-top-card 6)
  (flip-top-card 7)
  (deal-cards 0 '(2 3 4 5 6 7))
  (flip-top-card 2)
  (flip-top-card 3)
  (flip-top-card 4)
  (flip-top-card 5)
  (flip-top-card 6)
  (flip-top-card 7)
  (deal-cards 0 '(2 3 4 5 6 7))
  (flip-top-card 2)
  (flip-top-card 3)
  (flip-top-card 4)
  (flip-top-card 5)
  (flip-top-card 6)
  (flip-top-card 7)

  (list 9 4))

(define (button-pressed slot-id card-list)
  (if (and card-list
	   (> slot-id 0)
	   (< slot-id 8)
	   (is-visible? (car (reverse card-list))))
      #t
      #f))
      
(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (or (= end-slot 0)
	  (> end-slot 7))
      (add-to-score! 1))
  (if (not (empty-slot? start-slot))
      (make-visible-top-card start-slot)))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot) #f)
	((and (= (length card-list) 1)
	      (or (= end-slot 0)
		  (> end-slot 7)))
	 (cond ((and (= (get-value (car card-list)) ace)
		     (empty-slot? end-slot))
		#t)
	       ((and (not (empty-slot? end-slot))
		     (= (get-suit (get-top-card end-slot))
			(get-suit (car card-list)))
		     (= (+ 1 (get-value (get-top-card end-slot)))
			(get-value (car card-list))))
		#t)
	       (#t #f)))
	((and (> end-slot 0)
	      (< end-slot 8))
	 (cond ((and (empty-slot? end-slot)
		     (= (get-value (car (reverse card-list))) king))
		#t)
	       ((empty-slot? end-slot) #f)
	       ((and (eq? (is-black? (car (reverse card-list)))
			  (is-red? (get-top-card end-slot)))
		     (= (get-value (get-top-card end-slot))
			(+ 1 (get-value (car (reverse card-list))))))
		#t)
	       (#t #f)))
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot)
  (cond ((or (empty-slot? slot)
	     (= slot 0)
	     (> slot 7))
	 #f)
	((= (get-value (get-top-card slot)) ace)
	 (let ((top-card (get-top-card slot)))
	   (remove-card slot)
	   (cond ((empty-slot? 0)
		  (complete-transaction slot (list top-card) 0))
		 ((empty-slot? 8)
		  (complete-transaction slot (list top-card) 8))
		 ((empty-slot? 9)
		  (complete-transaction slot (list top-card) 9))
		 (#t
		  (complete-transaction slot (list top-card) 10)))))
      	((and (not (empty-slot? 0))
	      (= (get-suit (get-top-card 0))
		 (get-suit (get-top-card slot)))
	      (= (+ 1 (get-value (get-top-card 0)))
		 (get-value (get-top-card slot))))
	 (let ((top-card (get-top-card slot)))
	   (remove-card slot)
	   (complete-transaction slot (list top-card) 0)))
	((and (not (empty-slot? 8))
	      (= (get-suit (get-top-card 8))
		 (get-suit (get-top-card slot)))
	      (= (+ 1 (get-value (get-top-card 8)))
		 (get-value (get-top-card slot))))
	 (let ((top-card (get-top-card slot)))
	   (remove-card slot)
	   (complete-transaction slot (list top-card) 8)))
	((and (not (empty-slot? 9))
	      (= (get-suit (get-top-card 9))
		 (get-suit (get-top-card slot)))
	      (= (+ 1 (get-value (get-top-card 9)))
		 (get-value (get-top-card slot))))
	 (let ((top-card (get-top-card slot)))
	   (remove-card slot)
	   (complete-transaction slot (list top-card) 9)))
	((and (not (empty-slot? 10))
	      (= (get-suit (get-top-card 10))
		 (get-suit (get-top-card slot)))
	      (= (+ 1 (get-value (get-top-card 10)))
		 (get-value (get-top-card slot))))
	 (let ((top-card (get-top-card slot)))
	   (remove-card slot)
	   (complete-transaction slot (list top-card) 10)))
	(#t #f)))

(define (game-over)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (if (and (= 13 (length (get-cards 0)))
	   (= 13 (length (get-cards 8)))
	   (= 13 (length (get-cards 9)))
	   (= 13 (length (get-cards 10))))
      #t
      #f))

(define (here-kingy-kingy slot num-cards card-list)
  (cond ((or (= (length card-list) 0)
	     (= (length card-list) 1)
	     (not (is-visible? (car card-list))))
	 #f)
	((= (get-value (car card-list)) king)
	 (hint-move slot num-cards (find-empty-slot tableau)))
	(#t (here-kingy-kingy slot (+ num-cards 1) (cdr card-list)))))

(define (king-avail? slot-id)
  (cond ((= slot-id 8)
	 #f)
	((and (not (empty-slot? slot-id))
	      (here-kingy-kingy slot-id 1 (get-cards slot-id)))
	 (here-kingy-kingy slot-id 1 (get-cards slot-id)))
	(#t (king-avail? (+ 1 slot-id)))))

(define (check-for-empty)
  (and (find-empty-slot tableau)
       (king-avail? 1)))

(define (check-a-foundation card slot-id)
  (cond ((= slot-id 11)
	 #f)
	((= slot-id 1)
	 (check-a-foundation card 8))
	((and (not (empty-slot? slot-id))
	      (eq? (get-suit card)
		   (get-suit (get-top-card slot-id)))
	      (= (get-value card)
		 (+ 1 (get-value (get-top-card slot-id)))))
	 #t)
	(#t (check-a-foundation card (+ 1 slot-id)))))

(define (find-suit suit slots)
  (if (and (not (empty-slot? (car slots)))
           (= (get-suit (get-top-card (car slots))) suit))
      (car slots)
      (find-suit suit (cdr slots))))

(define (check-to-foundations? slot-id)
  (cond ((= slot-id 8)
	 #f)
	((empty-slot? slot-id)
	 (check-to-foundations? (+ 1 slot-id)))
	((= (get-value (get-top-card slot-id)) ace)
	 (hint-move slot-id 1 (find-empty-slot foundation)))
	((check-a-foundation (get-top-card slot-id) 0)
	 (hint-move slot-id 1 (find-suit (get-suit (get-top-card slot-id)) foundation)))
	(#t (check-to-foundations? (+ 1 slot-id)))))

(define (stripped card-list card)
  (if (<= (length card-list) 1)
      '()
      (if (eq? card (car card-list))
	  (cdr card-list)
	  (if (= (length card-list) 2)
	      '()
	      (stripped (cdr card-list) card)))))

(define (check-a-tableau card slot1 card-list slot2 num-cards)
  (cond ((or (= (length card-list) 0)
	     (not (is-visible? (car card-list))))
	 #f)
	((and (not (eq? (is-red? (car card-list))
			(is-red? card)))
	      (= (+ 1 (get-value (car card-list)))
		 (get-value card)))
	 (if (or  (= (length card-list) 1)
                  (not (is-visible? (cadr card-list)))
		  (eq? (is-red? (car card-list))
		       (is-red? (cadr card-list)))
		  (not (= (+ 1 (get-value (car card-list)))
			  (get-value (cadr card-list))))
		  (check-a-foundation (cadr card-list) 0)
		  (check-a-tableau (get-top-card slot2)
				   slot1	
				   (cdr card-list)
				   slot2
				   1)
		  (check-a-tableau (cadr card-list)
				   slot2
				   (get-cards slot1)
				   slot1
				   1)
		  (check-a-tableau (cadr card-list)
				   slot2
				   (stripped (get-cards slot2)
					     (car card-list))
				   slot2
				   1))
	     (hint-move slot2 num-cards slot1)
	     (check-a-tableau card 
			      slot1 
			      (cdr card-list) 
			      slot2
			      (+ num-cards 1))))
	(#t (check-a-tableau card slot1 (cdr card-list) slot2 (+ num-cards 1)))))

(define (check-to-tableau? slot1 slot2)
  (cond ((= slot1 8)
	 #f)
	((or (= slot2 8)
	     (empty-slot? slot1))
	 (check-to-tableau? (+ 1 slot1) 1))
	((and (not (= slot1 slot2))
	      (check-a-tableau (get-top-card slot1) 
			       slot1 
			       (get-cards slot2) 
			       slot2
			       1))
	 (check-a-tableau (get-top-card slot1) 
			  slot1 
			  (get-cards slot2) 
			  slot2
			  1))
	(#t (check-to-tableau? slot1 (+ 1 slot2)))))

(define (get-hint)
  (or (check-to-foundations? 1)
      (check-to-tableau? 1 2)
      (check-for-empty)))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
