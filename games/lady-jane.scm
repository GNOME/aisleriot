; AisleRiot - lady_jane.scm
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

(define BASE-VAL 0)

(define stock 0)
(define waste 1)
(define foundation '(2 3 4 5))
(define tableau '(6 7 8 9 10 11 12))
(define reserve '(13 14 15 16 17 18 19))

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

  (set! HORIZPOS 0)
  (set! VERTPOS 0)

  (set! VERTPOS (+ VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 7))
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 7))
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 7))
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 7))


  (set! HORIZPOS 0)
  (set! VERTPOS 0)

  (set! HORIZPOS (+ HORIZPOS 7))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 7))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 7))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 7))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)

  (deal-cards 0 '(7 8 9 10 11 12 8 9 10 11 12 9 10 11 12 10 11 12
		    11 12 12))
  (deal-cards-face-up 0 '(6 7 8 9 10 11 12 13 14 15 16 17 18 19 2))

  (add-to-score! 1)

  (set! BASE-VAL (get-value (get-top-card 2)))

  (give-status-message)

  (list 9 4)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-base-string))))

(define (get-base-string)
  (cond ((and (> BASE-VAL 1)
	      (< BASE-VAL 11))
	 (string-append (G_"Base Card:") " " (number->string BASE-VAL)))
	((= BASE-VAL 1)
	 (G_"Base Card: Ace"))
	((= BASE-VAL 11)
	 (G_"Base Card: Jack"))
	((= BASE-VAL 12)
	 (G_"Base Card: Queen"))
	((= BASE-VAL 13)
	 (G_"Base Card: King"))
	(#t "")))

(define (get-stock-no-string)
  (if (> (length (get-cards 0)) 1)
      (string-append (G_"Stock left:") " " 
		     (number->string (length (get-cards 0))))
      (string-append (G_"Stock left: 0")))) 

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (is-visible? (car (reverse card-list)))))

(define (to-foundation? card end-slot)
  (if (empty-slot? end-slot)
      (= (get-value card) BASE-VAL)
      (and (eq? (get-suit card)
		(get-suit (get-top-card end-slot)))
	   (or (= (+ 1 (get-value (get-top-card end-slot)))
		  (get-value card))
	       (and (= (get-value (get-top-card end-slot)) king)
		    (= (get-value card) ace))))))

(define (to-tableau? card end-slot)
  (if (empty-slot? end-slot)
      (or (= (get-value card) (- BASE-VAL 1))
	  (and (= BASE-VAL ace)
	       (= (get-value card) king)))
      (and (not (eq? (is-red? card)
		     (is-red? (get-top-card end-slot))))
	   (not (= (get-value (get-top-card end-slot)) BASE-VAL))
	   (or (= (get-value (get-top-card end-slot))
		  (+ 1 (get-value card)))
	       (and (= (get-value (get-top-card end-slot)) ace)
		    (= (get-value card) king))))))

(define (droppable? start-slot card-list end-slot)
  (if (not (= start-slot end-slot))
      (cond ((and (> end-slot 1)
		  (< end-slot 6))
	     (and (= (length card-list) 1)
		  (to-foundation? (car card-list) end-slot)))
	    ((and (> end-slot 5)
		  (< end-slot 13))
	     (and (to-tableau? (car (reverse card-list)) end-slot)))
	    (#t #f))
      #f))

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (cond ((and (> end-slot 1)
		  (< end-slot 6))
	     (and (or (and (> start-slot 5)
			   (< start-slot 13)
			   (not (empty-slot? start-slot))
			   (make-visible-top-card start-slot))
		      (and (> start-slot 1)
			   (< start-slot 6)
			   (add-to-score! -1))
		      #t)
		  (add-to-score! 1)
		  (move-n-cards! start-slot end-slot card-list)))
	    ((and (> end-slot 5)
		  (< end-slot 13))
	     (and (or (and (> start-slot 1)
			   (< start-slot 6)
			   (add-to-score! -1))
		      (and (> start-slot 5)
			   (< start-slot 13)
			   (not (empty-slot? start-slot))
			   (make-visible-top-card start-slot))
		      #t)
		  (move-n-cards! start-slot end-slot card-list)))
	    (#t #f))
      #f))

(define (button-clicked slot-id)
  (if (= slot-id 0)
      (cond ((> (length (get-cards slot-id)) 7)
	     (and (deal-cards-face-up 0 '(13 14 15 16 17 18 19))
		  (give-status-message)))
	    ((> (length (get-cards slot-id)) 1)
	     (and (deal-cards-face-up 0 '(1))
		  (make-visible-top-card 0)
		  (give-status-message)))
	    (#t #f))
      #f))

(define (move-to-foundations? card slot-id)
  (cond ((> slot-id 5)
	 #f)
	((to-foundation? card slot-id)
	 (add-card! slot-id card))
	(#t
	 (move-to-foundations? card (+ 1 slot-id)))))

(define (button-double-clicked slot-id)
  (if (or (empty-slot? slot-id)
	  (and (> slot-id 2)
	       (< slot-id 6))
	  (not (is-visible? (get-top-card slot-id))))
      #f
      (and (move-to-foundations? (get-top-card slot-id) 2)
	   (remove-card slot-id)
	   (add-to-score! 1)
	   (or (empty-slot? slot-id)
	       (> slot-id 12)
	       (< slot-id 2)
	       (make-visible-top-card slot-id)))))

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)))

(define (dealable?)
  (and (> (length (get-cards 0)) 1)
       (list 0 (G_"Deal another round"))))

(define (check-a-foundation slot1 slot2)
  (and (< slot2 6)
       (if (to-foundation? (get-top-card slot1) slot2)
           (hint-move slot1 1 slot2)
           (check-a-foundation slot1 (+ 1 slot2)))))

(define (check-to-foundations slot-id)
  (cond ((> slot-id 19)
	 #f)
	((= slot-id 2)
	 (check-to-foundations 6))
	((or (empty-slot? slot-id)
	     (not (is-visible? (get-top-card slot-id))))
	 (check-to-foundations (+ 1 slot-id)))
	(#t
	 (or (check-a-foundation slot-id 2)
	     (check-to-foundations (+ 1 slot-id))))))

(define (check-a-foundation2 card slot2)
  (if (< slot2 6)
      (or (to-foundation? card slot2)
	  (check-a-foundation2 card (+ 1 slot2)))
      #f))

(define (stripped card-list card)
  (if (<= (length card-list) 1)
      '()
      (if (eq? card (car card-list))
	  (cdr card-list)
	  (if (= (length card-list) 2)
	      '()
	      (stripped (cdr card-list) card)))))

(define (check-a-tableau-with-pile card slot1 card-list slot2 imbedded?)
  (cond ((or (= (length card-list) 0)
	     (not (is-visible? (car card-list))))
	 #f)
	((and (not (eq? (is-red? (car card-list))
			(is-red? card)))
	      (or (= (+ 1 (get-value (car card-list)))
		     (get-value card))
		  (and (= (get-value (car card-list))
			  king)
		       (= (get-value card)
			  ace))))
	 (if (or  (= (length card-list) 1)
		  (eq? (is-red? (car card-list))
		       (is-red? (cadr card-list)))
		  imbedded?
		  (not (and (is-visible? (cadr card-list))
			    (or (= (+ 1 (get-value (car card-list)))
				   (get-value (cadr card-list)))
				(and (= (get-value (car card-list))
					king)
				     (= (get-value (cadr card-list))
					ace)))))
		  (check-a-foundation2 (cadr card-list) 2)
		  (check-a-tableau-with-pile (get-top-card slot2)
					     slot1	
					     (cdr card-list)
					     slot2
					     #t)
		  (check-a-tableau-with-pile (cadr card-list)
					     slot2
					     (get-cards slot1)
					     slot1
					     #t)
		  (check-a-tableau-with-pile (cadr card-list)
					     slot2
					     (stripped (get-cards slot2)
						       (car card-list))
					     slot2
					     #t))
	     (if imbedded?
	         #t
	         (hint-move slot2 (- (+ 1 (length (get-cards slot2))) (length card-list)) slot1))
	     (and (not imbedded?)
		  (check-a-tableau-with-pile card 
					     slot1 
					     (cdr card-list) 
					     slot2 
					     imbedded?))))
	(imbedded? #f)
	(#t (check-a-tableau-with-pile card slot1 (cdr card-list) slot2 imbedded?))))

(define (check-a-tableau r-slot t-slot)
  (if (and (eq? (is-red? (get-top-card r-slot))
		(is-black? (get-top-card t-slot)))
	   (or (= (+ 1 (get-value (get-top-card r-slot)))
		  (get-value (get-top-card t-slot)))
	       (and (= (get-value (get-top-card r-slot))
		       king)
		    (= (get-value (get-top-card t-slot))
		       ace))))
      (hint-move r-slot 1 t-slot)
      #f))

(define (check-to-tableau? slot1 slot2)
  (cond ((= slot1 20)
	 #f)
	((= slot1 2)
	 (check-to-tableau? 6 7))
	((or (= slot2 13)
	     (empty-slot? slot1)
	     (not (is-visible? (get-top-card slot1))))
	 (check-to-tableau? (+ 1 slot1) 6))
	((and (not (= slot1 slot2))
	      (> slot1 5)
	      (< slot1 13)
	      (check-a-tableau-with-pile (get-top-card slot1) 
					 slot1 
					 (get-cards slot2) 
					 slot2 
					 #f))
	 (check-a-tableau-with-pile (get-top-card slot1) 
				    slot1 
				    (get-cards slot2) 
				    slot2 
				    #f))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (or (> slot1 12)
		  (< slot1 2))
	      (check-a-tableau slot1 slot2))
	 (check-a-tableau slot1 slot2))
	(#t (check-to-tableau? slot1 (+ 1 slot2)))))

(define (get-top-visible-card card-list)
  (if (not (is-visible? (cadr card-list)))
      (car card-list)
      (get-top-visible-card (cdr card-list))))

(define (visible-card-count card-list acc)
  (if (not (is-visible? (cadr card-list)))
      acc
      (visible-card-count (cdr card-list) (+ 1 acc))))

(define (find-high-value slot)
  (cond ((= slot 20)
	 #f)
	((= slot 2)
	 (find-high-value 6))
	((and (not (empty-slot? slot))
	      (is-visible? (get-top-card slot))
	      (< slot 13)
	      (> slot 5)
	      (not (is-visible? (car (reverse (get-cards slot)))))
	      (or (= (get-value (get-top-visible-card (get-cards slot)))
		     (- BASE-VAL 1))
		  (and (= (get-value (get-top-visible-card (get-cards slot)))
			  king)
		       (= BASE-VAL ace))))
	 (hint-move slot (visible-card-count (get-cards slot) 1) (find-empty-slot tableau)))
	((and (not (empty-slot? slot))
	      (or (> slot 12)
		  (< slot 2))
              (is-visible? (get-top-card slot))   
	      (or (= (get-value (get-top-card slot))
		     (- BASE-VAL 1))
		  (and (= (get-value (get-top-card slot))
			  king)
		       (= BASE-VAL ace))))
	 (hint-move slot 1 (find-empty-slot tableau)))
	(#t (find-high-value (+ 1 slot)))))

(define (empty-tableau?)
  (if (or (empty-slot? 6)
	  (empty-slot? 7)
	  (empty-slot? 8)
	  (empty-slot? 9)
	  (empty-slot? 10)
	  (empty-slot? 11)
	  (empty-slot? 12))
      (find-high-value 0)
      #f))

(define (get-hint)
  (or (check-to-foundations 0)
      (check-to-tableau? 0 6)
      (empty-tableau?)
      (dealable?)
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
