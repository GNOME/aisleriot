; AisleRiot - carpet.scm
; Copyright (C) 2005 Vincent Povirk <madewokherd@gmail.com>
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

(define (tableau? slot-id)
  (< 5 slot-id))
(define foundation '(2 3 4 5))
(define stock 0)
(define waste 1)

(define (remove-aces cards foundation-ids remaining-cards)
  (if (eq? cards '())
      remaining-cards
      (if (= (get-value (car cards)) ace)
          (and (move-n-cards! stock (car foundation-ids) (list (make-visible (car cards))))
               (remove-aces (cdr cards) (cdr foundation-ids) remaining-cards))
          (remove-aces (cdr cards) foundation-ids (cons (car cards) remaining-cards)))))

(define (deal-cards-to-tableau slot-id)
  (and (< slot-id 26)
       (deal-cards-face-up stock (list slot-id))
       (deal-cards-to-tableau (+ slot-id 1))))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot '())
  (add-normal-slot '())

  (add-blank-slot)
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
  
  (set-cards! stock (remove-aces DECK foundation '()))

  (deal-cards-to-tableau 6)

  (give-status-message)

  (list 7 5)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (or (tableau? slot-id)
      (= slot-id waste)))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (add-to-score! 1)
  (or (not (tableau? start-slot))
      (and (not (empty-slot? waste))
           (move-n-cards! waste start-slot (list (get-top-card waste)))
           (remove-n-cards waste 1))
      (and (not (empty-slot? stock))
           (move-n-cards! stock start-slot (list (make-visible (get-top-card stock))))
           (remove-n-cards stock 1))
  #t))

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (complete-transaction start-slot card-list end-slot) 
  #f))

(define (droppable? start-slot card-list end-slot)
  (and (member end-slot foundation)
       (= (get-suit (car card-list)) (get-suit (get-top-card end-slot)))
       (= (get-value (car card-list)) (+ 1 (get-value (get-top-card end-slot))))))

(define (dealable?)
  (flippable? stock waste 0))

(define (do-deal-next-cards)
  (flip-stock stock waste 0 1))

(define (button-clicked start-slot)
  (and (= start-slot stock)
       (do-deal-next-cards)))

(define (play-to-foundation start-slot end-slots)
  (if (or (eq? end-slots '())
          (empty-slot? start-slot))
      #f
      (if (droppable? start-slot (list (get-top-card start-slot)) (car end-slots))
          (letrec ((card (get-top-card start-slot)))
            (remove-n-cards start-slot 1)
            (complete-transaction start-slot (list card) (car end-slots)))
          (play-to-foundation start-slot (cdr end-slots)))))

(define (button-double-clicked slot-id)
  (and (or (= waste slot-id)
           (tableau? slot-id))
       (play-to-foundation slot-id foundation)))

(define (hint-to-foundation start-slot end-slots)
  (if (or (eq? end-slots '())
          (empty-slot? start-slot))
      #f
      (if (droppable? start-slot (list (get-top-card start-slot)) (car end-slots))
          (hint-move start-slot 1 (car end-slots))
          (hint-to-foundation start-slot (cdr end-slots)))))

(define (hint-tableau-to-foundation start-slot)
  (if (= start-slot 26)
      #f
      (or (hint-to-foundation start-slot foundation)
          (hint-tableau-to-foundation (+ start-slot 1)))))

(define (hint-flip-stock)
  (and (not (empty-slot? stock))
       (list 0 (G_"Deal a new card from the deck"))))

(define (get-hint)
  (or (hint-tableau-to-foundation 6)
      (hint-to-foundation waste foundation)
      (hint-flip-stock)))

(define (game-won)
  (and (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))
       (= 13 (length (get-cards 5)))))

(define (game-over)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (get-options)
  #f)

(define (apply-options options)
  #f)

(define (timeout) #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable? dealable?)
