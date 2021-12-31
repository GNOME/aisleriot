; AisleRiot - forty_thieves.scm
; Copyright (C) 2008 Ed Sirett  <ed@makewrite.demon.co.uk>
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

(define stock 0)
(define foundation '(1 2 3 4 5 6 7 8))
(define waste 9)
(define tableau '(10 11 12 13 14 15 16 17 18 19))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-double-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)

  (add-blank-slot)
; the foundations
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)

  (add-carriage-return-slot)
; the waste pile
  (add-extended-slot '() right 'waste)
  (add-carriage-return-slot)

; the tableau
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

; these are the forty theives in the tableau
  (deal-cards-face-up 0 tableau)
  (deal-cards-face-up 0 tableau)
  (deal-cards-face-up 0 tableau)
  (deal-cards-face-up 0 tableau)

  (give-status-message)
; this is the return list of (new-game) and sets the size of the 
; the playing field.
  (list 10 4.5)
)

(define (in-tableau? slot) 
  (and (>= slot 10) (<= slot 19))
)

(define (in-foundation? slot) 
  (and (>= slot 1) (<= slot 8))
)

(define (in-tableau-or-waste? slot) 
  (or (in-tableau? slot) (= slot waste-pile))
)

(define waste-pile 9)
(define stock-pile 0) 
(define start-with-waste 9)
(define start-with-tableau 10)

(define (<> a b) 
   (not (= a b))
)

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))
  )
)

; Apparently this is used to allow a group of cards to be dragged. 
; if it returns #t then the cards are picked.
; single cards can always be pulled from waste or tableau
; multiple cards must be straight suit descending
; (droppable?) will sort out more restrictions later
 (define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (in-tableau-or-waste? slot-id)
       ( or (= (length card-list) 1)
       	    (and (in-tableau? slot-id)
       	         (check-straight-descending-list card-list)
                 (check-same-suit-list card-list)	
	    )
       )
  )
)

;scoring  5*cards + 13 per suit completed.
(define (foundation-score slot-id prev-total)
  (define (current-total)
    (+ prev-total
       (* (length (get-cards slot-id)) 5)
       (if (= (length (get-cards slot-id)) 13)
           60
           0)))
  (if (= slot-id 8)
      (current-total)
      (foundation-score (+ slot-id 1) (current-total))))
       
(define (recalculate-score)
  (set-score!  (foundation-score 1 0)))

; counts empty slots in tableau
(define (space-score slot-id prev)
 (define (curtot previous) (+ previous (if (empty-slot? slot-id) 1 0)))
 (if (= slot-id 19) (curtot prev) (space-score (+ slot-id 1) (curtot prev)))
)
(define (tableau-spaces) 
   (space-score start-with-tableau 0)
)

; To save effort a pile of correctly descending same suit cards can be moved
; from the tableau to a foundation in one go.

( define (foundation-droppable? card-list f-slot) 
   (and (check-same-suit-list card-list)
        (check-straight-descending-list card-list) 
	(cond ( (empty-slot? f-slot)  
                    (= (get-value (car card-list)) ace) 
              )
              (	( = (get-value (car card-list)) (+ (get-value (get-top-card f-slot)) 1))
		    ( = (get-suit (get-top-card f-slot)) (get-suit (car card-list)))
	      )	
              (else #f)
	)
   )
)

; the maximum number of cards you can move as a short cut in one go 
; depends on the number of free tableau slot (it's 2^tableau slots)
; if the pile is going to an empty-slot than that slot is not really 
; an empty slot. If the pile is the entire contents of a tableau slot
; then (tableau-spaces) reports a 'false' extra space. hence the 
; extra code.
( define (max-move-in-tableau from-slot to-slot)
    (expt 2 (max 0 
                 (- 
		    (- (tableau-spaces) (if (empty-slot? to-slot) 1 0))
		       (if (empty-slot? from-slot) 1 0)
		 )
            )		
    )
)

; A bunch of cards may be dropped on to a tableau slot iff
; They are a descending same suit sequence that fits the top
; card of the tableau slot or an empty slot.
; this is a short cut to save moving cards individually
( define (tableau-droppable? s-slot card-list t-slot) 
   (and 
	(check-same-suit-list card-list)
        (check-straight-descending-list card-list)
	(<= (length card-list) (max-move-in-tableau s-slot t-slot))
	(cond ( (empty-slot? t-slot)  #t )
              (	( = (+ (get-value (car card-list)) (length card-list)) (get-value (get-top-card t-slot)) )
		    ( = (get-suit (get-top-card t-slot)) (get-suit (car card-list)))
	      )	
              (else #f) 
	)
   )
)


; droppable means that a list of cards coming from start-slot 
; and going to end-slot are valid to be moved. 
; picking up and dropping cards where they are is a null move.
; picking things off a foundation is a not permitted.
; dropping a valid pile onto a foundation is OK.
; if we are dropping onto another tableau pile sometimes OK.
; dropping card(s) elsewhere is not permitted.

(define (droppable?  start-slot card-list  end-slot) 
  (cond ( (= end-slot start-slot)  #f)
	( (in-foundation? start-slot) #f)
        ( (in-foundation? end-slot) (foundation-droppable? card-list end-slot) )
	( (in-tableau? end-slot) (tableau-droppable? start-slot card-list end-slot) )
	(else #f)
  )
)

;drop the dragged card(s) a pile of cards have to be revered 
; onto a foundation
(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (if (in-tableau? end-slot) 
             (move-n-cards! start-slot end-slot card-list)
             (move-n-cards! start-slot end-slot (reverse card-list) )
       )
   )
)

; return "a move" if a card can be moved from from-slot to a foundation
; a move is a list either (#f) or (#t from-slot to-slot)
; no cards are actually moved this is a helper for both double-click
; and get-hint features.

(define (try-all-foundations-helper from-slot card to-slots)
  (if (null? to-slots)
      (list #f)
      (if (foundation-droppable? (list card) (car to-slots))
          (list #t from-slot (car to-slots))
          (try-all-foundations-helper from-slot card (cdr to-slots)))))

(define (try-all-foundations from-slot card )
  (if (not (empty-slot? from-slot))
      (try-all-foundations-helper from-slot card foundation)
      (list #f)))


; return a move if a card can be moved from from-slot to a tableau
; slot. This is a helper for hint, and double-click

(define (find-tableau-place-helper from-slot card to-slots)
  (if (null? to-slots)
      #f
      (if (and
            (not (empty-slot? (car to-slots)))
            (tableau-droppable? from-slot (list card) (car to-slots))
            (<> from-slot (car to-slots)))
          (list #t from-slot (car to-slots))
          (find-tableau-place-helper from-slot card (cdr to-slots)))))

(define (find-tableau-place from-slot card )
  (if (not (empty-slot? from-slot))
      (or
        (find-tableau-place-helper from-slot card tableau)
        (and (find-empty-slot tableau) (list #t from-slot (find-empty-slot tableau)))
        (list #f))
      (list #f)))


(define (dealable?)
   (not (empty-slot? stock-pile)))

;deals cards from deck to waste
(define (button-clicked slot-id)
  (and (= slot-id stock-pile)
       (dealable?)
       (deal-cards-face-up stock-pile (list waste-pile))
  )
)

(define (do-deal-next-cards)
  (button-clicked stock-pile))

; if we can find a move to the foundations do it and return #t or #f.
(define (move-to-foundation) 
       (let ((move (find-any-move-to-foundation waste-pile))) 
	  (if (car move) (deal-cards-face-up (car (cdr move)) (list (car (reverse move))) ) #f ) 
       )
)

; search for any valid move to a foundation 
; helper code for both hint, autoplay
(define (find-any-move-to-foundation begin-slot) 
  (if (in-tableau-or-waste? begin-slot)
        (let ((test (try-all-foundations begin-slot (get-top-card begin-slot)) ))
             (if (car test) 
                 test 
                 (find-any-move-to-foundation (+ begin-slot 1)) 
             )
        )
        (list #f) 	
  )
)

; search for any valid move around the tableau 
; helper code for hint
(define (find-any-move-in-tableau begin-slot) 
  (if (in-tableau-or-waste? begin-slot)
        (let ((test (find-tableau-place begin-slot (get-top-card begin-slot)) ))
             (if (car test) 
                 test 
                 (find-any-move-in-tableau (+ begin-slot 1)) 
             )
        )
        (list #f) 	
  )
)



(define (autoplay-foundations)
(if (move-to-foundation) (delayed-call autoplay-foundations) #f)
)

; double click foundation for autoplay, otherwise does auto
; single move to foundation, or waste to tableau if poss.
(define (button-double-clicked slot-id)
  (cond ( (in-foundation? slot-id ) (autoplay-foundations))
        ( (in-tableau-or-waste? slot-id) 
            (let ((test (try-all-foundations slot-id (get-top-card slot-id)) ))
	      (if (car test) 
                 (deal-cards-face-up (car (cdr test)) (list (car (reverse test))) ) 
                 (let ((jump (find-tableau-place slot-id (get-top-card slot-id)) ))
                    (if (car jump) 
                       (deal-cards-face-up (car (cdr jump)) (list (car (reverse jump))) )
                       #f
                    )
                 )
              ) 
            )
          )
	(else #f)
   )
)


(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)
  )
)



(define (game-won)
  (= (recalculate-score) 1000)
)


;this is the last-straw hint maker
(define (check-for-deal)
  (if (not (empty-slot? stock-pile)) 
        (list 0 (G_"Deal a card from stock"))
	 #f
  )
)


(define (make-hint move)
    (if (car move)
       (hint-move (car (cdr move)) 1 (car (reverse move)))
       (list 0 "Bug! make-hint called on false move.")
    )
)



; hint  suggests the following in order:
;  a move to a foundation from waste or tableau
;  move the top waste card to a valid tableau space or pile
;  move some other tableau card to another tableau space or pile 
;  deal a card or at end backup and try alternatives.
; these are not intended to be a the best moves simply to show 
; possible moves to help learn the rules.
(define (get-hint)
  (cond ( (car (find-any-move-to-foundation start-with-waste))
          (make-hint (find-any-move-to-foundation start-with-waste)) 
        ) 
        ( (and (not (empty-slot? waste-pile)) 
               (car (find-tableau-place waste-pile (get-top-card waste-pile) ) ) 
          )
          (make-hint (find-tableau-place waste-pile (get-top-card waste-pile)))
        )
        ( (car (find-any-move-in-tableau start-with-tableau) ) 
          (make-hint (find-any-move-in-tableau start-with-tableau ) )
        )
        (else (check-for-deal))
  )
)

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
