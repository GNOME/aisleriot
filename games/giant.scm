; AisleRiot - giant.scm
; Copyright (C) 2009 Ed Sirett <ed@makewrite.demon.co.uk>
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


;set up the deck
(set-ace-low)

(define stock-slot 0)
(define foundation '(1 2 3 4 5 6 7 8))
(define tableau '(9 10 11 12 13 14 15 16 ))
(define reserve-slot 17)
(define (make-deck)
  (make-standard-double-deck)
)

(define winning-score 104)

(define allow-empty-slots #t)
(define same-suit #f)

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
  (add-blank-slot)
  (add-normal-slot '() 'reserve)

  (deal-cards-face-up stock-slot tableau)


  (give-status-message)
  (list 10 4.5))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string))
)

(define (get-stock-no-string)
  (format #f
          (G_"Deals left: ~a")
          (number->string (/ (length (get-cards stock-slot)) 8 ))
  )
)

;additional functions.

(define (complete-transaction start-slot card-list end-slot)
  (if (member end-slot foundation)
      (move-n-cards! start-slot end-slot (reverse card-list))
      (move-n-cards! start-slot end-slot card-list)
  )
)

(define (button-pressed slot card-list)
  (if (or (empty-slot? slot) (= slot stock-slot))
        #f   ; can't pick from stock or empty piles
        (and (or (and (not same-suit) (check-alternating-color-list card-list))
                 (and same-suit  (check-same-suit-list card-list)))
             (check-straight-descending-list card-list))))



(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       ( or (and  (member end-slot foundation)
                  (check-straight-descending-list card-list)
                  (check-same-suit-list card-list)
                  (if (empty-slot? end-slot)
                      (= (get-value (car card-list)) ace)
                      (and (= (get-suit (car card-list)) (get-suit (get-top-card end-slot)))
                           (= (- (get-value (car card-list)) 1 ) (get-value (get-top-card end-slot)))
                      )
                  )
            )
            (and  (member end-slot tableau)
                  (check-straight-descending-list card-list)
                  (or (and (not same-suit) (check-alternating-color-list card-list))
                      (and  same-suit (check-same-suit-list card-list)))
                  (if (not (empty-slot? end-slot))
                      (and (= (+ (get-value (car (reverse card-list))) 1 ) (get-value (get-top-card end-slot)))
                           (or (and (not same-suit)
                                    (not ( eq? ( is-red? ( car (reverse card-list))) (is-red? (get-top-card end-slot)))))
                               (and same-suit
                                    (= (get-suit (car (reverse card-list))) (get-suit (get-top-card end-slot))))))
                      #t
                  )
            )
            (and  (=  end-slot reserve-slot)
                  (empty-slot? reserve-slot)
                  (= (length card-list) 1)
            )
       )
  )
)

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot))
)

(define (do-deal-next-cards)
  (deal-cards-face-up stock-slot tableau))

(define (button-clicked slot)
  (if (= stock-slot slot)
      (if (dealable?) (do-deal-next-cards) #f)
      #f))


(define (find-any-to-foundation from-slots)
  (if (eq? from-slots '() )
      #f
      (let ((find-to-result (find-to foundation (car from-slots))))
        (if find-to-result
            (list (car from-slots) find-to-result)
            (find-any-to-foundation (cdr from-slots))))))

; remake a list of slots with/without empty members
(define (without-gaps slots with-empties)
    (cond ((eq? slots '()) '())
          (with-empties slots)
          ((empty-slot? (car slots)) (without-gaps (cdr slots) with-empties))
          ( else (cons (car slots) (without-gaps (cdr slots) with-empties)))))


(define (find-any-to-tableau from-slots with-empties)
  (if (eq? from-slots '() )
      #f
      (let ((find-to-result (find-to (without-gaps tableau with-empties) (car from-slots)))
            (cfs (car from-slots)))
        (if (and find-to-result
                  ; check we are not breaking an existing run
                 (or (= (length (get-cards cfs )) 1)
                     (not (check-straight-descending-list (list (get-top-card cfs) (cadr (get-cards cfs))))))
                  ; if suggesting a move to a gap make sure it is worthwhile
                 (or (not (empty-slot? find-to-result))
                     (> (length (get-cards cfs )) 1)))  ;can move a top card to a gap if it does not make a gap
            (list cfs find-to-result)
            (find-any-to-tableau (cdr from-slots) with-empties)))))

(define (move-any-to-foundation slots)
  (let (( find-any-result (find-any-to-foundation slots)))
    (if find-any-result
        (move-a-card (car find-any-result) (cadr find-any-result))
        #f)))


(define (auto-play)
    (if (move-any-to-foundation (append tableau (list reserve-slot)))
        (delayed-call auto-play)
        #f
    )
)


(define (find-to slots from-slot)
  (if (or (empty-slot? from-slot) (eq? slots '()))
        #f
       (if (droppable? from-slot (list (get-top-card from-slot)) (car slots) )
           (car slots)
           (find-to (cdr slots) from-slot)
       )
  )
)

(define (move-a-card from-slot to-slot)
   (if ( or (not to-slot) (empty-slot? from-slot))
        #f
       (add-card! to-slot (remove-card from-slot))
   )
)

(define (move-to-foundation from-slot)
   (move-a-card from-slot (find-to foundation from-slot ))
)


(define (button-double-clicked slot)
   (if (member slot foundation)
           (auto-play)
           (if (or (member slot tableau) (= slot reserve-slot) )
               (move-to-foundation slot)
               #f
           )
   )
)


(define (game-over)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))



; score the game - 1 pt for every card in the foundations 104 to win.
(define (game-score slot-list)
  (if (and (null? slot-list))
      0
      (+ (length (get-cards (car slot-list))) (game-score (cdr slot-list)))
  )
)

; game is won when all cards are moved to foundations.
(define (game-won)
   (= (set-score! (game-score foundation)) winning-score)
)



(define (dealable?)
  (if (and
        (not (empty-slot? stock-slot ))
        (or allow-empty-slots
            (not (any-slot-empty? tableau))))
      (list 0 (G_"Deal a row"))
      #f))



; This is the hint function
; 1) Suggest a move to a foundation.
; 2) Suggest moving a card from the (reserve  + tableau) to the tableau.
; 3) Suggest moviing a card to an empty tableau-slot
; 4) Suggest moving to the reserve if unoccupied
; 5) Suggest dealing a row if there are cards still in the stock.
; 6) Suggest moving cards around.

(define (get-hint)
  (let ((find-result (find-any-to-foundation (append tableau (list reserve-slot))))
        (t-result1   (find-any-to-tableau  (append tableau (list reserve-slot)) #f  ))
        (t-result2   (find-any-to-tableau  (append tableau (list reserve-slot)) #t )))
     (cond
           ( find-result
            (hint-move (car find-result) 1 (cadr find-result)))
           ( t-result1
            (hint-move (car t-result1) 1 (cadr t-result1)))
           ( t-result2
            (hint-move (car t-result2) 1 (cadr t-result2)))
           ( (empty-slot? reserve-slot) (list 0 (G_"Try moving a card to the reserve")))
           ( (dealable?) (list 0 (G_"Try dealing a row of cards")))
; this isn't great, but it will get around the premature end-of-game call
           (else (list 0 (G_"Try moving card piles around")))
     )))

(define (get-options)
  (list 'begin-exclusive
        (list (G_"Same suit") same-suit)
        (list (G_"Alternating colors") (not same-suit))
        'end-exclusive))

(define (apply-options options)
  (set! same-suit (cadr (list-ref options 1))))

(define (timeout) #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint
get-options apply-options timeout droppable? dealable?)
