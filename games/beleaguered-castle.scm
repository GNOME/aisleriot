; AisleRiot - Beleaguered Castle
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

(use-modules (aisleriot interface) (aisleriot api))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (set! DECK (make-deck-list-ace-low 2 2 club))
  (shuffle-deck)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot DECK)
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-extended-slot '() right)
  (deal-cards-face-up 1 '(0 2 3 5 6 8 9 11 0 2 3 5 6 8 9 11 
			  0 2 3 5 6 8 9 11 0 2 3 5 6 8 9 11 
			  0 2 3 5 6 8 9 11 0 2 3 5 6 8 9 11))
  (add-card! 1 (make-visible (make-card ace club)))
  (add-card! 4 (make-visible (make-card ace diamond)))
  (add-card! 7 (make-visible (make-card ace heart)))
  (add-card! 10 (make-visible (make-card ace spade)))

  (list 9 4)
)

(define (button-pressed slot-id card-list)
  (and (= (length card-list) 1)
       (not (= ace (get-value (car card-list))))))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((member end-slot '(1 4 7 10))
	 (and (= (get-suit (get-top-card end-slot))
		 (get-suit (car card-list)))
	      (= (+ 1 (get-value (get-top-card end-slot)))
		 (get-value (car card-list)))))
	(#t (or (empty-slot? end-slot)
	        (= (get-value (get-top-card end-slot))
	           (+ 1 (get-value (car card-list))))))))
	
(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (or (not (member end-slot '(1 4 7 10)))
           (add-to-score! 1))
       (move-n-cards! start-slot end-slot card-list)
       (or (not (member start-slot '(1 4 7 10)))
           (add-to-score! -1))))

(define (button-clicked slot-id) 
  #f)

(define (button-double-clicked slot-id)
  (if (and (not (empty-slot? slot-id))
	   (is-visible? (get-top-card slot-id))
	   (not (or (= slot-id 1)
		    (= slot-id 4)
		    (= slot-id 7)
		    (= slot-id 10))))
      (cond ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 1))))
	     (if (and (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 1)))))
		 (begin
		   (deal-cards slot-id '(1))
		   (add-to-score! 1))
		 #f))
	    ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 4))))
	     (if (and (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 4)))))
		 (begin
		   (deal-cards slot-id '(4))
		   (add-to-score! 1))
		 #f))
	    ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 7))))
	     (if (and (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 7)))))
		 (begin
		   (deal-cards slot-id '(7))
		   (add-to-score! 1))
		 #f))
	    ((and (= (get-suit (get-top-card slot-id))
		     (get-suit (get-top-card 10))))
	     (if (and (= (get-value (get-top-card slot-id))
			 (+ 1 (get-value (get-top-card 10)))))
		 (begin
		   (deal-cards slot-id '(10))
		   (add-to-score! 1))
		 #f))
	    (#t #f))
      #f))
	    
(define (game-continuable)
  (not (game-won)))

(define (game-won)
  (and (= (length (get-cards 1)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 7)) 13)
       (= (length (get-cards 10)) 13)))

(define (check-a-foundation slot1 slot2)
  (if (and (= (get-suit (get-top-card slot1))
	      (get-suit (get-top-card slot2)))
	   (= (get-value (get-top-card slot1))
	      (+ 1 (get-value (get-top-card slot2)))))
      (hint-move slot1 1 slot2)
      #f))

(define (check-to-foundation slot-id)
  (if (not (or (empty-slot? slot-id)
	       (= slot-id 1)
	       (= slot-id 4)
	       (= slot-id 7)
	       (= slot-id 10)))
      (or (check-a-foundation slot-id 1)
	  (check-a-foundation slot-id 4)
	  (check-a-foundation slot-id 7)
	  (check-a-foundation slot-id 10)
	  (check-to-foundation (+ 1 slot-id)))
      (if (< slot-id 10)
	  (check-to-foundation (+ 1 slot-id))
	  #f)))

(define (get-hint)
  (or (check-to-foundation 0)
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
