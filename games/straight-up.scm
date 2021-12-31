; AisleRiot - straight_up.scm
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

(define (new-game)
  (initialize-playing-area)
  (set-ace-high)

  (set! DECK (make-deck-list-ace-high 3 3 club))
  (shuffle-deck)

  (add-normal-slot DECK 'stock)
  (add-normal-slot '() 'waste)

  (add-blank-slot)

  (add-normal-slot (list (make-visible (make-card 2 club))) 'foundation)
  (add-normal-slot (list (make-visible (make-card 2 diamond))) 'foundation)
  (add-normal-slot (list (make-visible (make-card 2 heart))) 'foundation)
  (add-normal-slot (list (make-visible (make-card 2 spade))) 'foundation)

  (add-carriage-return-slot)

  (add-normal-slot '() 'reserve)

  (add-blank-slot)
  (add-blank-slot)

  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (deal-cards 0 '(6 6 6 6 6 6 6 6 6 6 6 6))
  (deal-cards-face-up 0 '(6 7 8 9 10))

  (give-status-message)

  (list 7 3)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-reserve-no-string)
					"   "
					(get-redeals-string))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		  (number->string (length (get-cards 0)))))

(define (get-reserve-no-string)
  (string-append (G_"Reserve left:") " "
		 (number->string (length (get-cards 6)))))

(define (get-redeals-string)
  (string-append (G_"Redeals left:") " "
		 (number->string (- 2 FLIP-COUNTER))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 1)
	   (> slot-id 5))))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot) #f)
        ((and (> end-slot 1)
              (< end-slot 6))
         (and (eq? (get-suit (get-top-card end-slot))
                   (get-suit (car card-list)))
              (= (+ 1 (get-value (get-top-card end-slot)))
                 (get-value (car card-list)))))
        ((> end-slot 6)
         (or (and (empty-slot? end-slot)
                  (empty-slot? 6)
                  (= start-slot 1))
             (and (not (empty-slot? end-slot))
                  (eq? (get-suit (get-top-card end-slot))
                       (get-suit (car card-list)))
                  (= (get-value (get-top-card end-slot))
                     (+ 1 (get-value (car (reverse card-list))))))))
        (else #f)))

(define (button-released start-slot card-list end-slot)
  (cond ((= start-slot end-slot) #f)
        ((and (> end-slot 1)
	      (< end-slot 6))
	 (and (eq? (get-suit (get-top-card end-slot))
		   (get-suit (car card-list)))
	      (= (+ 1 (get-value (get-top-card end-slot)))
		 (get-value (car card-list)))
	      (add-to-score! (length card-list))
	      (move-n-cards! start-slot end-slot (reverse card-list))
	      (check-reserve start-slot)))
	((> end-slot 6)
	 (or (and (empty-slot? end-slot)
		  (empty-slot? 6)
		  (= start-slot 1)
		  (move-n-cards! start-slot end-slot card-list))
	     (and (not (empty-slot? end-slot))
		  (eq? (get-suit (get-top-card end-slot))
		       (get-suit (car card-list)))
		  (= (get-value (get-top-card end-slot))
		     (+ 1 (get-value (car (reverse card-list)))))
		  (move-n-cards! start-slot end-slot card-list)
		  (check-reserve start-slot))))
	(else #f)))

(define (check-reserve start-slot)
  (begin 
    (or (< start-slot 6)
	(empty-slot? 6)
	(and (= 6 start-slot)
	     (make-visible-top-card 6))
	(not (empty-slot? start-slot))
	(and (deal-cards 6 (list start-slot))
	     (or (empty-slot? 6)
		 (make-visible-top-card 6))))
    (give-status-message)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (flip-stock 0 1 2)
       (give-status-message)))

(define (check-up slot-id foundation-id)
  (if (eq? (get-suit (get-top-card slot-id))
	   (get-suit (get-top-card foundation-id)))
      (and (= (get-value (get-top-card slot-id))
	      (+ 1 (get-value (get-top-card foundation-id))))
	   (move-n-cards! slot-id 
			  foundation-id 
			  (list (get-top-card slot-id)))
	   (add-to-score! 1)
	   (remove-card slot-id)
	   (check-reserve slot-id))
      (check-up slot-id (+ 1 foundation-id))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (is-visible? (get-top-card slot-id))
       (check-up slot-id 2)))

(define (dealable?)
  (flippable? 0 1 2))
  
(define (do-deal-next-cards)
  (and (flip-stock 0 1 2)
       (give-status-message)))

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)))

(define (check-a-foundation slot-id foundation-id)
  (cond ((= foundation-id 6)
	 #f)
	((eq? (get-suit (get-top-card slot-id))
	      (get-suit (get-top-card foundation-id)))
	 (and (= (get-value (get-top-card slot-id))
	         (+ 1 (get-value (get-top-card foundation-id))))
	      foundation-id))
	(#t (check-a-foundation slot-id (+ 1 foundation-id)))))	

(define (to-foundations slot-id)
  (cond ((= slot-id 11)
	 #f)
	((= slot-id 2)
	 (to-foundations 6))
	((and (not (empty-slot? slot-id))
	      (check-a-foundation slot-id 2))
	 (hint-move slot-id 1 (check-a-foundation slot-id 2)))
	(#t
	 (to-foundations (+ 1 slot-id)))))

(define (check-a-tableau slot-id t-slot)
  (cond ((= t-slot 11)
	 #f)
	((and (not (empty-slot? t-slot))
	      (not (= slot-id t-slot))
	      (eq? (get-suit (get-top-card slot-id))
		   (get-suit (get-top-card t-slot)))
	      (or (and (< slot-id 7)
		       (= (get-value (get-top-card t-slot))
			  (+ 1 (get-value (get-top-card slot-id)))))
		  (and (> slot-id 6)
		       (= (get-value (get-top-card t-slot))
			  (+ 1 
			     (get-value 
			      (car (reverse (get-cards slot-id)))))))))
	 t-slot)
	(#t (check-a-tableau slot-id (+ 1 t-slot)))))

(define (to-tableau slot-id)
  (cond ((= slot-id 11)
	 #f)
	((= slot-id 2)
	 (to-tableau 6))
	((and (not (empty-slot? slot-id))
	      (check-a-tableau slot-id 7))
	 (if (< slot-id 7)
	     (hint-move slot-id 1 (check-a-tableau slot-id 7))
	     (hint-move slot-id (length (get-cards slot-id)) (check-a-tableau slot-id 7))))
	(#t (to-tableau (+ 1 slot-id)))))

(define (empty-tableau? slot-id)
  (cond ((or (empty-slot? 1)
	     (> slot-id 10))
	 #f)
	((empty-slot? slot-id)
	 (hint-move 1 1 slot-id))
	(#t (empty-tableau? (+ 1 slot-id)))))

(define (get-hint)
  (or (to-foundations 1)
      (to-tableau 1)
      (empty-tableau? 7)
      (if (not (empty-slot? 0))
          (list 0 (G_"Deal a new card from the deck"))
          (if (and (< FLIP-COUNTER 2)
                   (not (empty-slot? 1)))
              (list 0 (G_"Move waste back to stock"))
              #f))))

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
