; AisleRiot - neighbor.scm
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
  (make-standard-deck)
  (shuffle-deck)
 
  (add-normal-slot DECK) 
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (add-normal-slot '()) 
  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
			    19 20 21 22 23 24 25))

  (give-status-message)

  (list 7 5))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (= slot-id 0))
       (not (empty-slot? slot-id))
       (not (= (get-value (get-top-card slot-id)) king))))

(define (fill-it-up slot-id spaces)
  (cond ((> (+ slot-id spaces) 25)
	 (begin
	   (if (not (empty-slot? 0))
	       (deal-cards-face-up 0 (cons slot-id '()))
	       #t)
	   (if (< slot-id 25)
	       (fill-it-up (+ slot-id 1) spaces)
	       #t)))
	((not (empty-slot? (+ slot-id spaces)))
	 (begin
	   (deal-cards-face-up (+ slot-id spaces) (cons slot-id '()))
	   (if (< slot-id 25)
	       (fill-it-up (+ slot-id 1) spaces)
	       #t)))
	(#t
	 (fill-it-up slot-id (+ 1 spaces)))))

(define (droppable? start-slot card-list end-slot)
  (let ((dx (- (modulo (- start-slot 1) 5)
               (modulo (- end-slot 1) 5)))
        (dy (- (quotient (- start-slot 1) 5)
               (quotient (- end-slot 1) 5))))
       (and (not (= start-slot end-slot))
            (not (= end-slot 0))
            (not (empty-slot? end-slot))
            (= 13 (+ (get-value (car card-list))
                     (get-value (get-top-card end-slot))))
            (member dx '(-1 0 1))
            (member dy '(-1 0 1)))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (add-to-score! +2)
       (remove-card end-slot)
       (fill-it-up (min start-slot end-slot) 1)))
	 
(define (button-clicked slot-id)  
  (if (and (not (empty-slot? slot-id))
	   (= (get-value (get-top-card slot-id)) king))
      (begin
	(add-to-score! +1)
	(remove-card slot-id)
	(fill-it-up slot-id 1))
      #f))

(define (button-double-clicked slot)
  #f)     

(define (game-won)
  (empty-slot? 1))

(define (game-over)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (hint-remove-king suit)
  (cond ((eq? suit club) (G_"Remove the king of clubs."))
        ((eq? suit diamond) (G_"Remove the king of diamonds."))
        ((eq? suit heart) (G_"Remove the king of hearts."))
        ((eq? suit spade) (G_"Remove the king of spades."))))

(define (king-check slot-id)
  (cond ((= king (get-value (get-top-card slot-id)))
	 (hint-click slot-id (hint-remove-king (get-suit (get-top-card slot-id)))))
	((and (< slot-id 25)
	      (not (empty-slot? (+ 1 slot-id))))
	 (king-check (+ 1 slot-id)))
	(#t #f)))

(define (horizontal-check slot-id)
  (cond ((and (not (= 0 (modulo slot-id 5)))
	      (= 13 (+ (get-value (get-top-card slot-id))
		       (get-value (get-top-card (+ 1 slot-id))))))
	 (hint-move slot-id 1 (+ 1 slot-id)))
	((and (< slot-id 24)
	      (not (empty-slot? (+ 2 slot-id))))
	 (horizontal-check (+ 1 slot-id)))
	(#t #f)))

(define (vertical-check slot-id)
  (cond ((= 13 (+ (get-value (get-top-card slot-id))
		  (get-value (get-top-card (+ 5 slot-id)))))
	 (hint-move slot-id 1 (+ 5 slot-id)))
	((and (< slot-id 19)
	      (not (empty-slot? (+ 6 slot-id))))
	 (vertical-check (+ 1 slot-id)))
	(#t #f)))	       

(define (backslash-check slot-id)
  (cond ((and (not (= 0 (modulo slot-id 5)))
	      (= 13 (+ (get-value (get-top-card slot-id))
		       (get-value (get-top-card (+ 6 slot-id))))))
	 (hint-move slot-id 1 (+ 6 slot-id)))
	((and (not (empty-slot? (+ 7 slot-id)))
	      (< slot-id 18))
	 (backslash-check (+ 1 slot-id)))
	(#t #f)))

(define (slash-check slot-id)
  (cond ((and (not (= 1 (modulo slot-id 5)))
	      (= 13 (+ (get-value (get-top-card slot-id))
		       (get-value (get-top-card (+ 4 slot-id))))))
	 (hint-move slot-id 1 (+ 4 slot-id)))
	((and (< slot-id 19)
	      (not (empty-slot? (+ 5 slot-id))))
	 (slash-check (+ 1 slot-id)))
	(#t #f)))

(define (get-hint)
  (or (king-check 1)
      (and (not (empty-slot? 2))
	   (horizontal-check 1))
      (and (not (empty-slot? 6))
	   (vertical-check 1))
      (and (not (empty-slot? 7))
	   (backslash-check 1))
      (and (not (empty-slot? 6))
	   (slash-check 2))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
