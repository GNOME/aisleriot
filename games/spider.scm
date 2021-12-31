; AisleRiot - spider.scm
; Copyright (C) 1998 Jonathan Blandford <jrb@mit.edu>
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

(define suits-one #f)
(define suits-two #f)
(define suits-four #t)

;set up the deck
(set-ace-low)

(define stock 0)
(define foundation '(1 2 3 4 5 6 7 8))
(define tableau '(9 10 11 12 13 14 15 16 17 18))
(define initial-deal '(9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 10 11 12 13 14 15 16 17 18 9 12 15 18))
(define (make-deck)
  (cond
    (suits-one (set! DECK (append (make-standard-deck-list-ace-low ace spade)
		     (make-standard-deck-list-ace-low ace spade) 
		     (make-standard-deck-list-ace-low ace spade) 
		     (make-standard-deck-list-ace-low ace spade) 
		     (make-standard-deck-list-ace-low ace spade) 
		     (make-standard-deck-list-ace-low ace spade) 
		     (make-standard-deck-list-ace-low ace spade) 
		     (make-standard-deck-list-ace-low ace spade))))
    (suits-two (set! DECK (append (make-standard-deck-list-ace-low ace heart)
		     (make-standard-deck-list-ace-low ace heart) 
		     (make-standard-deck-list-ace-low ace heart) 
		     (make-standard-deck-list-ace-low ace heart))))
    (else (make-standard-double-deck))))

(define winning-score 96)

(define allow-empty-slots #f)

(define (new-game)
  (initialize-playing-area)
  (make-deck)
  (shuffle-deck)

  ;set up the board
  (add-normal-slot DECK 'stock)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
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
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (deal-initial-setup)

  (give-status-message)
  
  (list 10 4))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (format #f
          (G_"Stock left: ~a")
	  (number->string (length (get-cards stock)))))

;internal procedures/variables

(define (flip-top-card-slots slots)
  (or (eq? slots '())
      (and (flip-top-card (car slots))
           (flip-top-card-slots (cdr slots)))))

(define (deal-initial-setup)
  (deal-cards stock initial-deal)
  (flip-top-card-slots tableau))

;additional functions.

(define (complete-transaction start-slot card-list end-slot)
  (if (and (not (empty-slot? start-slot))
	   (is-visible? (get-top-card start-slot))
	   (eq? (get-suit (get-top-card start-slot))
		(get-suit (car (reverse card-list))))
	   (= (get-value (get-top-card start-slot))
	      (+ 1 (get-value (car (reverse card-list))))))
      (add-to-score! -1))
  (if (and (not (empty-slot? end-slot))
	   (is-visible? (get-top-card end-slot))
	   (eq? (get-suit (get-top-card end-slot))
		(get-suit (car (reverse card-list))))
	   (= (get-value (get-top-card end-slot))
	      (+ 1 (get-value (car (reverse card-list))))))
      (add-to-score! 1))
  (move-n-cards! start-slot end-slot card-list)
  (if (and (not (empty-slot? start-slot)) (member start-slot tableau))
      (make-visible-top-card start-slot)
      #f)
  #t)

(define (check-for-points slot)
  (and (> (length (get-cards slot)) 1)
       (is-visible? (cadr (get-cards slot)))
       (eq? (get-suit (get-top-card slot))
            (get-suit (cadr (get-cards slot))))
       (= (+ 1 (get-value (get-top-card slot)))
          (get-value (cadr (get-cards slot))))
       (add-to-score! 1)))

(define (deal-new-cards slots)
  (and (not (eq? slots '()))
       (> (length (get-cards stock)) 0)
       (begin
          (deal-cards-face-up stock (list (car slots)))
          (check-for-points (car slots))
          (deal-new-cards (cdr slots)))))

(define (button-pressed slot card-list)
  (give-status-message)
  (if (or (empty-slot? slot)
	  (= slot stock)
	  (member slot foundation))
      #f
      (if (not (eq? '() card-list))
	  (if (is-visible? (car (reverse card-list)))
	      (if (check-same-suit-list card-list)
		  (if (check-straight-descending-list card-list)
		      #t
		      #f)
		  #f)
	      #f)
	  #f)))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (if (empty-slot? end-slot)
 	   (or (and (member end-slot foundation) 
		    (= 13 (length card-list)))
	       (member end-slot tableau))
	   (and (member end-slot tableau)
	        (= (get-value (get-top-card end-slot))
	        (+ (get-value (car (reverse card-list))) 1))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (must-undo-to-deal?)
  (and (not allow-empty-slots)
       (not (empty-slot? stock))
       (< (count-cards tableau 0) (length tableau))))

(define (button-clicked slot)
  (and (= stock slot)
       (not (empty-slot? stock))
       (if (and (not allow-empty-slots)
                (any-slot-empty? tableau))
	   (begin
	     (if (must-undo-to-deal?)
	         (set-statusbar-message (G_"Undo until there are enough cards to fill all tableau piles"))
	         (set-statusbar-message (G_"Please fill in empty pile first.")))
             #f)
	   (begin
	     (deal-new-cards tableau)
	     (give-status-message)
	     #t))))


(define (is-playable-stack cards suit n)
  (and (not (null? cards))
       (= (get-value (car cards)) n)
       (is-visible? (car cards))
       (eq? (get-suit (car cards)) suit)
       (or (= n 13)
           (is-playable-stack (cdr cards) suit (+ n 1)))))

(define (button-double-clicked slot)
  (and (member slot tableau)
       (not (empty-slot? slot))
       (is-playable-stack (get-cards slot) (get-suit (car (get-cards slot))) 1)
       (let ((card-list (list-head (get-cards slot) 13)))
            (remove-n-cards slot 13)
            (complete-transaction slot card-list (find-empty-slot foundation)))
       #t))

(define (game-over)
  (and (not (game-won))
       (get-hint)))

(define (all-slots-empty? slots)
  (or (eq? slots '())
      (and (empty-slot? (car slots))
           (all-slots-empty? (cdr slots)))))

; Game can be won on two conditions.  Either all the cards being moved
; to the top slots, or by stacking all the cards (score of 96)
(define (game-won)
  (or
   (and (= (get-score) winning-score)
        (all-slots-empty? foundation))
   (and (empty-slot? 0)
	(all-slots-empty? tableau))))

(define (depth-card card-list)
  (if (and (> (length card-list) 1)
	   (is-visible? (cadr card-list))
	   (eq? (get-suit (car card-list))
		(get-suit (cadr card-list)))
	   (eq? (+ 1 (get-value (car card-list)))
		(get-value (cadr card-list))))
      (depth-card (cdr card-list))
      card-list))

(define (depth-card-pos card-list acc)
  (if (and (> (length card-list) 1)
	   (is-visible? (cadr card-list))
	   (eq? (get-suit (car card-list))
		(get-suit (cadr card-list)))
	   (eq? (+ 1 (get-value (car card-list)))
		(get-value (cadr card-list))))
      (depth-card-pos (cdr card-list) (+ acc 1))
      acc))

(define (check-a-slot source card-to-move targets same-suit?)
  (if (eq? targets '())
      #f
      (if (and (not (= source (car targets)))
	       (not (empty-slot? (car targets)))
	       (eq? same-suit?
		    (eq? (get-suit card-to-move)
			 (get-suit (get-top-card (car targets)))))
	       (= (+ 1 (get-value card-to-move))
		  (get-value (get-top-card (car targets)))))
	  (hint-move source (depth-card-pos (get-cards source) 1) (car targets))
	  (check-a-slot source card-to-move (cdr targets) same-suit?))))

(define (same-suit-check slots)
  (if (eq? slots '())
      #f
      (if (and (not (empty-slot? (car slots)))
	       (check-a-slot (car slots) (car (depth-card (get-cards (car slots)))) tableau #t))
	  (check-a-slot (car slots) (car (depth-card (get-cards (car slots)))) tableau #t)
	  (same-suit-check (cdr slots)))))

(define (not-same-suit-check slots)
  (if (eq? slots '())
      #f
      (if (and (not (empty-slot? (car slots)))
	       (or (= 1 (length (depth-card (get-cards (car slots)))))
	           (not (is-visible? (cadr (depth-card (get-cards (car slots))))))
		   (not (eq? (+ 1 (get-value (car (depth-card (get-cards (car slots))))))
			     (get-value (cadr (depth-card (get-cards (car slots))))))))
	       (check-a-slot (car slots) (car (depth-card (get-cards (car slots)))) tableau #f))
	  (check-a-slot (car slots) (car (depth-card (get-cards (car slots)))) tableau #f)
	  (not-same-suit-check (cdr slots)))))

(define (open-slots? slots)
  (if (eq? slots '())
      #f
      (if (empty-slot? (car slots))
	  (list 0 (G_"Place something on empty slot"))
	  (open-slots? (cdr slots)))))

(define (dealable?)
  (if (not (empty-slot? stock))
      (list 0 (G_"Deal another round"))
      #f))

(define (count-cards slots acc)
  (if (null? slots)
      acc
      (count-cards (cdr slots) (+ acc (length (get-cards (car slots)))))))

(define (hint-few-tableau-cards)
  (and (must-undo-to-deal?)
       (list 0 (G_"Undo until there are enough cards to fill all tableau piles"))))

(define (get-hint)
  (or (hint-few-tableau-cards)
      (same-suit-check tableau)
      (not-same-suit-check tableau)
      (open-slots? tableau)
      (dealable?)
; this isn't great, but it will get around the premature end-of-game call
      (list 0 (G_"Try moving card piles around"))))

(define (get-options)
  (list 'begin-exclusive 
	(list (G_"Four Suits") suits-four)
	(list (G_"Two Suits") suits-two)
	(list (G_"One Suit") suits-one)
	'end-exclusive))

(define (apply-options options)
  (set! suits-four (cadr (list-ref options 1)))
  (set! suits-two (cadr (list-ref options 2)))
  (set! suits-one (cadr (list-ref options 3))))

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
