; Aisleriot - klondike.scm
; Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
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
(define deal-three #f)

(define (new-game)
  (initialize-playing-area)

  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK)
  (add-partially-extended-slot '() right 3)
;  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards 0 '(6 7 8 9 10 11 12 7 8 9 10 11 12 8 9 10 11 12 9 10 11 12 10 11 12 11 12 12))
  
  (flip-top-card 6)
  (flip-top-card 7)
  (flip-top-card 8)
  (flip-top-card 9)
  (flip-top-card 10)
  (flip-top-card 11)
  (flip-top-card 12)
  (set! FLIP-COUNTER 0)
  (list 7 3)
)

(define (button-pressed slot-id card-list)
  (and (or (> slot-id 1)
	   (and (= slot-id 1)
		(= (length card-list) 1)))
       (is-visible? (car (reverse card-list)))))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (and (> start-slot 1)
	   (< start-slot 6))
      (add-to-score! -1))
  (if (and (> end-slot 1)
	   (< end-slot 6))
      (add-to-score! 1))
  (if (and (not (empty-slot? start-slot)) 
	   (> start-slot 5))
      (make-visible-top-card start-slot))
  #t)

(define (button-released start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (> end-slot 1)
       (if (> end-slot 5)
	   (if (empty-slot? end-slot)
	       (= king (get-value (car (reverse card-list))))
	       (and (not (eq? (is-red? (get-top-card end-slot))
			      (is-red? (car (reverse card-list)))))
		    (= (get-value (get-top-card end-slot))
		       (+ (get-value (car (reverse card-list))) 1))))
	   (and (= 1 (list-length card-list))
		(if (empty-slot? end-slot)
		    (= ace (get-value (car card-list)))
		    (and (= (get-suit (get-top-card end-slot))
			    (get-suit (car card-list)))
			 (= (get-value (get-top-card end-slot)) 
				(- (get-value (car card-list)) 1))))))
       (complete-transaction start-slot card-list end-slot)))

(define (flip-cards-back)
  (if (not (empty-slot? 1))
      (begin
	(add-card! 0 (flip-card (remove-card 1)))
	(flip-cards-back))))

(define (button-clicked start-slot)
  (or (and (= start-slot 0)
	   (if (empty-slot? start-slot)
	       (and (< FLIP-COUNTER 3)
		    (set! FLIP-COUNTER (+ 1 FLIP-COUNTER))
		    (flip-cards-back))
	       (begin
		 (add-card! 1 (flip-card (remove-card start-slot)))
		 (if deal-three
		   (begin
		     (and (not (empty-slot? start-slot))
			  (add-card! 1 (flip-card (remove-card start-slot))))
		     (and (not (empty-slot? start-slot))
			  (add-card! 1 (flip-card (remove-card start-slot)))))))))
      (and (or (> start-slot 5) (= start-slot 1))
	   (not (empty-slot? start-slot))
	   (let* ((card (get-top-card start-slot))
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
		    (add-card! end-slot (remove-card start-slot))
		    (not (empty-slot? start-slot))
		    (make-visible-top-card start-slot)))))))

; The C still has problems detecting button double clicks ... 
					 
(define (button-double-clicked slot)
  #f)

; The set up:

(define tableau '(6 7 8 9 10 11 12))
(define foundation '(2 3 4 5))
(define stock 0)
(define waste 1)

; Global variables used in searching (keeping it simple):

(define card #f)
(define color 0)
(define suit 0)
(define value 0)
(define slot-id1 0)

(define (match? slot-id2)
  (and (not (empty-slot? slot-id2))
       (= suit (get-suit (get-top-card slot-id2)))
       (= value (get-value (get-top-card slot-id2)))
       (list 1 (get-name (get-top-card slot-id2)) (get-name card))))

(define (ploppable? slot-id)
  (and (not (empty-slot? slot-id))
       (set! card (get-top-card slot-id))
       (set! suit (get-suit card))
       (set! value (+ (get-value card) 1))
       (or-map match? (cons waste tableau))))

(define (is-ace? slot-id)
  (and (not (empty-slot? slot-id))
       (= ace (get-value (get-top-card slot-id)))
       (list 2 (get-name (get-top-card slot-id)) "an empty slot" )))

(define (shiftable? slot-id2)
  (and (not (= slot-id2 slot-id1))
       (if (empty-slot? slot-id2)
	   (and (= value king)
		(list 2 (get-name card) "an empty slot"))
	   (and (= (get-value (get-top-card slot-id2)) (+ 1 value))
		(not (= (get-color (get-top-card slot-id2)) color))
		(list 1 (get-name card) (get-name (get-top-card slot-id2)))))))

(define (check-visible card)
  (and (is-visible? card) card))

(define (shiftable-iter slot-id)
  (and (not (empty-slot? slot-id))
       (let ((card-list (reverse (get-cards slot-id))))
	 (set! card (or-map check-visible card-list))
	 (set! color (get-color card))	
	 (set! value (get-value card))
	 (set! slot-id1 slot-id)
	 (and (not (and (= value king)
			(eq? card (car card-list))))
	      (or-map shiftable? tableau)))))

(define (addable? slot-id)
  (if (empty-slot? slot-id)
      (and (= (get-value card) king)
	   (list 2 (get-name card) "an empty slot" ))
      (and (= (get-value (get-top-card slot-id)) (+ 1 (get-value card)))
	   (not (= (get-color (get-top-card slot-id)) (get-color card)))
	   (list 1 (get-name card) (get-name (get-top-card slot-id))))))

(define (get-hint)
  (or (or-map is-ace? (cons waste tableau))
      (or-map shiftable-iter tableau)
      (and (not (empty-slot? waste))
	   (set! card (get-top-card waste))
	   (or-map addable? tableau))
      (or-map ploppable? foundation)
      (and (or (and (< FLIP-COUNTER 3)
		    (not (empty-slot? waste)))
	       (not (empty-slot? stock))) 
	   (list 0 "Deal a new card from the deck"))
; FIXME: need to give proper hints for this case too ...
      (list 0 "Try moving cards down from the foundation")))

(define (game-won)
  (and (= 13 (list-length (get-cards 2)))
       (= 13 (list-length (get-cards 3)))
       (= 13 (list-length (get-cards 4)))
       (= 13 (list-length (get-cards 5)))))

; The hints still miss some useful reversible moves:
;
; 1) unplopping cards to assist in shifting groups,
; 2) unplopping cards to assist in plopping cards in other suits, 
; 3) shifting groups to assist in plopping & unplopping cards.
;
; so we must NOT report game-over when they run out.

(define (game-over)
  (not (game-won)))

(define (get-options)
  (list (list "Three card deals" deal-three)))

(define (apply-options options)
  (set! deal-three (cadar options)))

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
