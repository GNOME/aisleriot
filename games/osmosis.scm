; AisleRiot - osmosis.scm
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

(use-modules (aisleriot interface) (aisleriot api) (ice-9 format))

(define deal-three #f)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-extended-slot '() right 'reserve)      ;Slot 0
  (add-blank-slot)
  (add-extended-slot '() right 'foundation)      ;Slot 1
  (add-carriage-return-slot)
  (add-extended-slot '() right 'reserve)      ;Slot 2
  (add-blank-slot)
  (add-extended-slot '() right 'foundation)      ;Slot 3
  (add-carriage-return-slot)
  (add-extended-slot '() right 'reserve)      ;Slot 4
  (add-blank-slot)
  (add-extended-slot '() right 'foundation)      ;Slot 5
  (add-carriage-return-slot)
  (add-extended-slot '() right 'reserve)      ;Slot 6
  (add-blank-slot)
  (add-extended-slot '() right 'foundation)      ;Slot 7
  (add-carriage-return-slot)
  (add-normal-slot DECK 'stock)             ;Slot 8

  (if deal-three
    (add-partially-extended-slot '() right 3 'waste)
    (add-normal-slot '() 'waste)             
  )                                  ;Slot 9

  (initial-deal)

  (give-status-message)

  (add-to-score! 1)
  (list 6 5))

(define (initial-deal)
  (deal-cards 8 '(0 2 4 6 0 2 4 6 0 2 4 6))
  (deal-cards-face-up 8 '(0 2 4 6 1))
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (get-stock-no-string)
  (format #f (G_"Stock left: ~a") (number->string (length (get-cards 8))))
)

(define (get-redeals-string)
  (if deal-three
    ""
    (format #f (G_"Redeals left: ~a") (number->string (- 2 FLIP-COUNTER)))
  )
)

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (= (length card-list) 1)
       (or (= slot-id 0) 
	   (= slot-id 2)
	   (= slot-id 4) 
	   (= slot-id 6)
	   (= slot-id 9))))
		
(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (add-to-score! 1)
  (if (not (empty-slot? start-slot))
      (make-visible-top-card start-slot))
  #t)

(define (find-card-val-in-list? cards value)
  (and (not (null? cards))
       (or (= value (get-value (car cards)))
	   (find-card-val-in-list? (cdr cards) value))))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (= end-slot 1)
	   (= end-slot 3)
	   (= end-slot 5)
	   (= end-slot 7))
       (if (empty-slot? end-slot)
	   (and (= (get-value (car (reverse (get-cards 1))))
		   (get-value (car card-list)))
		(while (empty-slot? (- end-slot 2)) 
		       (set! end-slot (- end-slot 2))))
	   (and (= (get-suit (get-top-card end-slot))
		   (get-suit (car card-list)))
		(or (= end-slot 1)
		    (find-card-val-in-list? (get-cards (- end-slot 2))
					    (get-value (car card-list))) )))))
  
(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))
  
(define (button-clicked slot-id)
  (and (= slot-id 8)
       (flip-stock 8 9 (if deal-three -1 2) (if deal-three 3 1))))

(define (check-to-move orig-slot end-slot above-list top-card)
  (if (not (null? above-list))
      (if (eq? (get-value top-card)
	       (get-value (car above-list)))
	  (begin
	    (remove-card orig-slot)
	    (complete-transaction orig-slot (list top-card) end-slot))
	  (check-to-move orig-slot end-slot (cdr above-list) top-card))
      #f))
  

(define (button-double-clicked slot)
  (if (and (or (= slot 0)
	       (= slot 2)
	       (= slot 4)
	       (= slot 6)
	       (= slot 9))
	   (not (empty-slot? slot)))
      (begin
	(let ((top-card (get-top-card slot)))
	  (if (eq? (get-suit top-card)
		   (get-suit (car (get-cards 1))))
	      (begin
		(remove-card slot)
		(complete-transaction slot (list top-card) 1))
	      (if (eq? (get-value top-card)
		       (get-value (car (reverse (get-cards 1)))))
		  (cond  ((empty-slot? 3)
			  (begin
			    (remove-card slot)
			    (complete-transaction slot (list top-card) 3)))
			 ((empty-slot? 5)
			  (begin
			    (remove-card slot)
			    (complete-transaction slot (list top-card) 5)))
			 (#t
			  (begin
			    (remove-card slot)
			    (complete-transaction slot (list top-card) 7))))
		  (cond ((and (not (empty-slot? 3))
			      (eq? (get-suit top-card)
				   (get-suit (car (get-cards 3)))))
			 (check-to-move slot 3 (get-cards 1) top-card))
			((and (not (empty-slot? 5))
			      (eq? (get-suit top-card)
				   (get-suit (car (get-cards 5)))))
			 (check-to-move slot 5 (get-cards 3) top-card))
			((and (not (empty-slot? 7))
			      (eq? (get-suit top-card)
				   (get-suit (car (get-cards 7)))))
			 (check-to-move slot 7 (get-cards 5) top-card))
			(#t #f))))))	 
      #f))

(define (placeable? from-slot card slot-id)
  (and (< slot-id 9)
       (or (if (empty-slot? slot-id)
	       (and (= (get-value card) 
		       (get-value (car (reverse (get-cards 1)))))
		    (hint-move from-slot 1 slot-id))
	       (and (= (get-suit card) (get-suit (get-top-card slot-id)))
		    (or (= slot-id 1)
			(find-card-val-in-list? (get-cards (- slot-id 2)) 
						(get-value card)))
		    (hint-move from-slot 1 slot-id)))
	   (placeable? from-slot card (+ slot-id 2)))))

(define (get-valid-move id-list)
  (and (not (null? id-list))
       (or (and (not (empty-slot? (car id-list)))
		(placeable? (car id-list) (get-top-card (car id-list)) 1))
	   (get-valid-move (cdr id-list)))))

(define (game-continuable)
  (give-status-message)
  (or (and (or deal-three
               (< FLIP-COUNTER 2))
	   (not (empty-slot? 9)))
      (not (empty-slot? 8))
      (get-valid-move '(0 2 4 6 9))))

(define (game-won)
  (and (= 13 (length (get-cards 1)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 5)))
       (= 13 (length (get-cards 7)))))

(define (get-hint)
  (or (get-valid-move '(0 2 4 6 9))
      (if deal-three
        (list 0 (G_"Deal new cards from the deck"))
        (list 0 (G_"Deal a new card from the deck"))
      )
  )
)

(define (get-options)
  (list (list (G_"Three card deals") deal-three)))

(define (apply-options options)
  (set! deal-three (cadar options))
)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-continuable game-won get-hint get-options apply-options timeout droppable?)
