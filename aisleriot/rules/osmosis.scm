; Aisleriot - osmosis.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>
;
; This game is free software; you can redistribute it and/or
; modify it under the terms of the GNU Library General Public
; License as published by the Free Software Foundation; either
; version 2 of the License, or (at your option) any later version.
;
; This library is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; Library General Public License for more details.
;
; You should have received a copy of the GNU Library General Public
; License along with this library; if not, write to the Free
; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(define FLIP-COUNTER 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-extended-slot '() right)      ;Slot 0
  (add-blank-slot)
  (add-extended-slot '() right)      ;Slot 1
  (add-carriage-return-slot)
  (add-extended-slot '() right)      ;Slot 2
  (add-blank-slot)
  (add-extended-slot '() right)      ;Slot 3
  (add-carriage-return-slot)
  (add-extended-slot '() right)      ;Slot 4
  (add-blank-slot)
  (add-extended-slot '() right)      ;Slot 5
  (add-carriage-return-slot)
  (add-extended-slot '() right)      ;Slot 6
  (add-blank-slot)
  (add-extended-slot '() right)      ;Slot 7
  (add-carriage-return-slot)
  (add-normal-slot DECK)             ;Slot 8
  (add-normal-slot '())              ;Slot 9

  (deal-cards 8 '(0 2 4 6 0 2 4 6 0 2 4 6))
  (deal-cards-face-up 8 '(0 2 4 6 1))
)
  
(define (button-pressed slot-id card-list)
  (if (empty-slot? slot-id)
		#f
		(if (or (= slot-id 8)
				  (odd?  slot-id )
				  (> (list-length card-list) 1))
			 (if (= slot-id 9)
				  #t
				  #f)
			 #t)))
		
(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (not (empty-slot? start-slot))
		(make-visible-top-card start-slot)
		#f)
  #t)

(define (find-card-val-in-list? search-list search-val)
  (if (null? search-list)
		#f
		(if (= search-val
				 (get-value (car search-list)))
			 #t
			 (find-card-val-in-list? (cdr search-list) search-val))))

(define (button-released start-slot card-list end-slot)
  (if (or (= start-slot end-slot)
			 (even? end-slot)
			 (= end-slot 9))
		#f
		(cond ((and (empty-slot? end-slot)
						(= (get-value (car (reverse (get-cards 1))))
							(get-value (car card-list))))
				 (begin
					(if (and (> end-slot 3) 
								(empty-slot? (- end-slot 2)))
						 (begin
							(set! end-slot (- end-slot 2))
							(if (empty-slot? (- end-slot 2))
								 (set! end-slot (- end-slot 2))))
						 #f)
					(complete-transaction start-slot card-list end-slot)))
				((empty-slot? end-slot) #f)
				((= (get-suit (get-top-card end-slot))
							(get-suit (car card-list)))
				 (if (= end-slot 1)
					  (complete-transaction start-slot card-list end-slot)
					  (if (find-card-val-in-list? (get-cards (- end-slot 2))
															(get-value (car card-list))) 
							(complete-transaction start-slot card-list end-slot)
							#f)))
				(#t #f))
		)
)
  
(define (flip-cards-back)
  (if (> FLIP-COUNTER 2)
		#f
		(if (empty-slot? 9)
			 #f
			 (begin
				(add-card! 8 (flip-card (remove-card 9)))
				(flip-cards-back)))))

(define (button-clicked slot-id)
  (if (= slot-id 8)
      (if (empty-slot? 8)
			 (begin
				(flip-cards-back)
				(set! FLIP-COUNTER (+ 1 FLIP-COUNTER)))
			 (let ((top-card (remove-card 8)))
				(if (eq? top-card '())
					 #f
					 (add-card! 9 (flip-card top-card)))))
      #f))

	      
(define (button-double-clicked slot)
  #f)


(define (placeable? card slot-id)
  (if (empty-slot? slot-id)
		(= (get-value card) (get-value (car (reverse (get-cards 1)))))
		(if (= (get-suit card) (get-suit (get-top-card slot-id)))
				(if (= slot-id 1)
					 #t
					 (if (find-card-val-in-list? (get-cards (- slot-id 2)) (get-value card))
						  #t
						  #f))
			 (placeable? card (+ 2 slot-id)))))
  


(define (get-valid-move id)
  (if (> id 9)
		#f
		(if (not (empty-slot? id))
			 (let ((test-card (get-top-card id)))
				(if (placeable? test-card 1)
					 test-card
					 (if (= id 6)
						  (get-valid-move 9)
						  (get-valid-move (+ 2 id)))))
			 (if (= id 6)
				  (get-valid-move 9)
				  (get-valid-move (+ 2 id))))))
			 


(define (game-over not-used)
  (if (and (or (> FLIP-COUNTER 2)
					(empty-slot? 9))
			  (empty-slot? 8)
			  (not (get-valid-move 0)))
		#f
		#t))

(define (game-won not-used)
  (if (and (= 13 (list-length (get-cards 1)))
			  (= 13 (list-length (get-cards 3)))
			  (= 13 (list-length (get-cards 5)))
			  (= 13 (list-length (get-cards 7))))
		#t
		#f))

(define (get-hint not-used)
  #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)

