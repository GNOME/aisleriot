; Aisleriot - odessa.scm
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


;set up the deck
(set-ace-low)


(define (new-game) 
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK)			;Slot 0
  (add-extended-slot '() down)		;Slot 1
  (add-extended-slot '() down)		;Slot 2
  (add-extended-slot '() down)		;Slot 3
  (add-extended-slot '() down)		;Slot 4
  (add-extended-slot '() down)		;Slot 5
  (add-extended-slot '() down)		;Slot 6
  (add-extended-slot '() down)		;Slot 7
  (add-carriage-return-slot)
  (add-normal-slot '())			;Slot 8
  (add-carriage-return-slot)
  (add-normal-slot '())			;Slot 9
  (add-carriage-return-slot)
  (add-normal-slot '())			;Slot 10
  
  (deal-cards 0 '(1 2 3 4 5 6 7 1 2 3 4 5 6 7  1 2 3 4 5 6 7 ))
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 1 2 3 4 5 6 7  1 2 3 4 5 6 7 2 3 4 5 6 2 3 4 5 6))
)


(define (button-pressed slot-id card-list)
  (if (empty-slot? slot-id)
      #f
      (if (or (< slot-id 1) (> slot-id 7))
	  #f
	  (if (not (eq? '() card-list))
	      (if (is-visible? (car (reverse card-list)))
				 #t
				 #f)))))

(define (num-in-a-row top-card rest-slot)
  (if (eq? '() rest-slot)
		0
		(if (and (eq? 1 (abs (- (get-value top-card)
										(get-value (car rest-slot)))))
					(eq? (get-suit top-card) (get-suit (car rest-slot))))
			 (+ 1 (num-in-a-row (car rest-slot) (cdr rest-slot)))
			 0)))

(define (extract-score cards find-card back-cards)
  (if (cards-eq? (car cards) find-card)
		(+ 1 
			(num-in-a-row find-card back-cards)
			(num-in-a-row find-card (cdr cards)))
		(extract-score (cdr cards) find-card (cons (car cards) back-cards))))
		
(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (add-to-score! (extract-score (get-cards end-slot) (car (reverse card-list)) '()))
  (if (not (empty-slot? start-slot)) 
      (make-visible-top-card start-slot)
      #f)
  #t)

(define (button-released start-slot card-list end-slot)
  (if (= start-slot end-slot)
      #f
      (if (empty-slot? end-slot)
			 (cond ((and (or (> end-slot 7) 
								  (= end-slot 0))
							 (check-same-suit-list card-list)
							 (check-straight-descending-list card-list)
							 (= ace (get-value (car card-list))))
					  (complete-transaction start-slot (reverse card-list) end-slot))
					 ((and (> end-slot 0)
							 (< end-slot 8)
							 (= king (get-value (car (reverse card-list)))))
					  (complete-transaction start-slot card-list end-slot))
					 (#t #f))
			 (cond ((and (> end-slot 0)
							 (< end-slot 8)
							 (= (get-suit (get-top-card end-slot))
								 (get-suit (car (reverse card-list))))
							 (= (get-value (get-top-card end-slot))
								 (+ (get-value (car (reverse card-list))) 1)))
					  (complete-transaction start-slot card-list end-slot)) 
					 ((and (or (= end-slot 0)
								  (> end-slot 7))
							 (check-same-suit-list card-list)
							 (check-straight-descending-list card-list)
							 (= (get-suit (get-top-card end-slot))
								 (get-suit (car (reverse card-list))))
							 (= (get-value (get-top-card end-slot))
								 (- (get-value (car card-list)) 1)))
					  (complete-transaction start-slot (reverse card-list) end-slot)) 
					 (#t #f)))))


(define (button-clicked slot)
  #f)
	      
(define (button-double-clicked slot)
  #f)


(define (is-ploppable card)
  (if (eq? '() card)
		#f
		(let ((retval #f))
		  (if (= ace (get-value card))
				(set! retval card))
		  (if (and (not (empty-slot? 0)) (not retval))
				(if (and (= (get-value card)
								(+ 1 (get-value (get-top-card 0))))
							(= (get-suit card)
								(get-suit (get-top-card 0))))
					 (begin
						(set! retval card))))
		  (if (and (not (empty-slot? 8)) (not retval))
				(if (and (= (get-value card)
								(+ 1 (get-value (get-top-card 8))))
							(= (get-suit card)
								(get-suit (get-top-card 8))))
					 (begin
						(set! retval card))))
		  (if (and (not (empty-slot? 9)) (not retval))
				(if (and (= (get-value card)
								(+ 1 (get-value (get-top-card 9))))
							(= (get-suit card)
								(get-suit (get-top-card 9))))
					 (begin
						(set! retval card))))
		  (if (and (not (empty-slot? 10)) (not retval))
				(if (and (= (get-value card)
								(+ 1 (get-value (get-top-card 10))))
							(= (get-suit card)
								(get-suit (get-top-card 10))))
					 (begin
						(set! retval card))))
		  retval)))
				  
						 
(define (game-over-helper slot-id)
  (if (= slot-id 8)
		#f
		(if (is-ploppable (get-top-card slot-id))
			 (list (get-top-card slot-id))
			 (if (is-visible-card (append 
										  (if (= slot-id 1)
												'()
												(get-cards 1))
										  (if (= slot-id 2)
												'()
												(get-cards 2))
										  (if (= slot-id 3)
												'()
												(get-cards 3))
										  (if (= slot-id 4)
												'()
												(get-cards 4))
										  (if (= slot-id 5)
												'()
												(get-cards 5))
										  (if (= slot-id 6)
												'()
												(get-cards 6))
										  (if (= slot-id 7)
												'()
												(get-cards 7)))
										 (get-top-card slot-id))
				  (list (get-top-card slot-id))
				  (game-over-helper (+ 1 slot-id))))))

(define (is-visible-card cards top-card)
  (if (or (eq? '() cards) (eq? '() top-card))
		#f
		(if (and (= (get-suit (car cards)) (get-suit top-card))
					(= (+ 1 (get-value (car cards))) (get-value top-card)))
			 (is-visible? (car cards))
			 (is-visible-card (cdr cards) top-card))))
		

(define (game-over ugh)
  (let ((temp (game-over-helper 1)))
	 temp))

(define (game-won ugh)
  (if (and (= 13 (list-length (get-cards 0)))
			  (= 13 (list-length (get-cards 8)))
			  (= 13 (list-length (get-cards 9)))
			  (= 13 (list-length (get-cards 10))))
		#t
		#f))

(define (get-hint ugh)
  (let ((hint (game-over #t)))
	 (display hint)(newline)
	 (list 1 (get-name (make-card (+ -1 (get-value (car hint))) (get-suit (car hint)))) (get-name (car hint)))))

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)


