; AisleRiot - cruel.scm
; Copyright (C) 2005 Zach Keene <zjkeene@bellsouth.net>
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

(define stock 0)
(define foundations '(1 2 3 4))
(define from-list '(5 6 7 8 9 10 11 12 13 14 15 16))
(define to-list '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
(define original-to-slots '())

(def-save-var just-redealt #t)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-normal-slot '())
  (add-normal-slot '())  
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (set-cards! stock (remove-aces DECK foundations '()))
  (set! just-redealt #t)
  (cruel-deal 0)
  (give-status)
  (list 6 3)
)

(define (remove-aces cards foundation-ids remaining-cards)
  (if (eq? cards '())
      remaining-cards
      (if (= (get-value (car cards)) ace)
          (and (move-n-cards! stock (car foundation-ids) (list (make-visible (car cards))))
               (remove-aces (cdr cards) (cdr foundation-ids) remaining-cards))
          (remove-aces (cdr cards) foundation-ids (cons (car cards) remaining-cards)))))

(define (cruel-deal count)
  (if (not (empty-slot? stock))
    (begin
      (deal-cards-face-up stock (list (+ 5 (quotient count 4))))
      (cruel-deal (+ 1 count))
    )
  )
)

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 4)
  )
)

(define (droppable? start-slot card-list end-slot)
  (if (< end-slot 5) 
    (and (not (= end-slot stock))
         (= (get-suit(get-top-card end-slot)) (get-suit(car card-list)))
         (= (+ 1 (get-value(get-top-card end-slot))) (get-value(car card-list)))
    )
    (and (not (= start-slot end-slot))
         (not (empty-slot? end-slot))
         (= (get-suit(get-top-card end-slot)) (get-suit(car card-list)))
         (= (- (get-value(get-top-card end-slot)) 1) (get-value(car card-list)))
    )
  )
)

(define (button-released start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (set! just-redealt #f)
       (if (< end-slot 5)
          (add-to-score! 1)
       )
  )
)

(define (dealable?)
  (not just-redealt)
)

(define (button-clicked slot-id)
  (and (= stock slot-id)
       (dealable?)
       (do-deal-next-cards)
  )
)

(define (do-deal-next-cards) 
  (for-each
    (lambda (x)
      (if (not (empty-slot? x))
         (flip-deck stock x)
      )
    )
    '(16 15 14 13 12 11 10 9 8 7 6 5)
  )
  (cruel-deal 0)
  (set! just-redealt #t)
)

(define (button-double-clicked slot-id)
  (if (and (not (empty-slot? slot-id)) (> slot-id 4))
      (attempt-foundation slot-id foundations)
      #f
  )
)

(define (attempt-foundation start-slot end-slots)
  (if (null? end-slots)
      #f
      (if (button-released start-slot
                           (list (get-top-card start-slot))
                           (car end-slots)
          )
          (remove-card start-slot)
          (attempt-foundation start-slot (cdr end-slots))
      )
  )
)

(define (give-status)
  (set-statusbar-message (format #f 
                                 (G_"Cards remaining: ~a") 
				 (number->string (- 48 (get-score)))))
                         
)

(define (game-continuable)
  (give-status)
  (and (not (game-won))
       (or (not just-redealt) (check-moves from-list to-list))
       (not (headbanger?))
  )
)

(define (count x y) 
  (if (< x y) 
      (cons x (count (+ x 1) y)) 
      (cons x '()))
)

; Detects the case where, after a redeal, the only possible move is from the
; last pile to the next-to-last pile when the last pile only contains one
; card. After this move, the only thing left to do is redeal again, which
; just leaves us where we started.
(define (headbanger?) 
  (define last-slot (+ 5 (quotient (- 48 (get-score)) 4)))
  (and (not (= 47 (get-score)))
       just-redealt
       (= 1 (modulo (- 48 (get-score)) 4))
       (droppable? last-slot (list (get-top-card last-slot)) (- last-slot 1))
       (not (check-moves (count 5 (- last-slot 1)) to-list))
  )
)

(define (game-won)
  (and (= 13 (length (get-cards 1)))
       (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))
  )
)

(define (get-hint)
  (or (check-moves from-list to-list)
      (list 0 (G_"Redeal."))
  )
)

(define (check-moves from-slots to-slots)
  (set! original-to-slots to-slots)
  (check-move-helper from-slots to-slots)
)

(define (check-move-helper from-slots to-slots)
  (if (null? from-slots)
      #f
      (if (null? to-slots)
          (check-move-helper (cdr from-slots) original-to-slots)
          (if (and (not (empty-slot? (car from-slots)))
                   (not (= (car from-slots) (car to-slots)))
                   (droppable? (car from-slots) 
                               (list (get-top-card(car from-slots)))
                               (car to-slots)
                   )
               )              
               (hint-move (car from-slots) 1 (car to-slots))
               (check-move-helper from-slots (cdr to-slots))
          )
      )
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
