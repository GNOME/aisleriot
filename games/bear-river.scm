; AisleRiot - Bear River
; Copyright (C) 2009 Vincent Povirk
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

(define tableau '(4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))
(define foundation '(0 1 2 3))
(define hole '(9 15 21))

(define BASE-VAL 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)

  (add-blank-slot)
  (add-normal-slot DECK 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (set! HORIZPOS (+ HORIZPOS 0.18))
  (add-extended-slot '() right 'tableau)
  (add-carriage-return-slot)

  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (set! HORIZPOS (+ HORIZPOS 0.18))
  (add-extended-slot '() right 'tableau)
  (add-carriage-return-slot)

  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (add-extended-slot '() right 'tableau)
  (set! HORIZPOS (+ HORIZPOS 0.18))
  (add-extended-slot '() right 'tableau)
  (add-carriage-return-slot)

  (deal-to-tableau 0 tableau)
  (flip-top-card 0)

  (set! BASE-VAL (get-value (get-top-card 0)))

  (list 6.3 4))

(define (deal-to-tableau deck piles)
  (if (null? piles)
      #t
      (begin
        (deal-cards-face-up deck (list (car piles) (car piles)))
        (and (not (member (car piles) hole))
             (deal-cards-face-up deck (list (car piles))))
        (deal-to-tableau deck (cdr piles)))))

(define (give-status-message)
  (set-statusbar-message (get-base-string)))

(define (get-base-string)
  (cond ((and (> BASE-VAL 1)
              (< BASE-VAL 11))
         (string-append (G_"Base Card: ") (number->string BASE-VAL)))
        ((= BASE-VAL 1)
         (G_"Base Card: Ace"))
        ((= BASE-VAL 11)
         (G_"Base Card: Jack"))
        ((= BASE-VAL 12)
         (G_"Base Card: Queen"))
        ((= BASE-VAL 13)
         (G_"Base Card: King"))
        (#t "")))

(define (button-pressed slot-id card-list)
  (and (member slot-id tableau)
       (= (length card-list) 1)))

(define (value-offset? offset card1 card2)
  (= offset
     (modulo (- (get-value card2) (get-value card1)) 13)))

(define (droppable? start-slot card-list end-slot)
  (if (member end-slot foundation)
      (if (empty-slot? end-slot)
          (= (get-value (car card-list)) BASE-VAL)
          (and (suit-eq? (car card-list) (get-top-card end-slot))
               (value-offset? 1 (get-top-card end-slot) (car card-list))))
      (and (not (= start-slot end-slot))
           (if (empty-slot? end-slot)
               (member end-slot hole)
               (and (< (length (get-cards end-slot)) 3)
                    (suit-eq? (get-top-card end-slot) (car card-list))
                    (or (value-offset? 1 (get-top-card end-slot) (car card-list))
                        (value-offset? 1 (car card-list) (get-top-card end-slot))))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)))

(define (button-clicked slot-id)
  #f)

(define (try-to-foundations from-slot to-slots)
  (if (null? to-slots)
      #f
      (if (droppable? from-slot (list (get-top-card from-slot)) (car to-slots))
          (deal-cards from-slot (list (car to-slots)))
          (try-to-foundations from-slot (cdr to-slots)))))

(define (button-double-clicked slot-id)
  (and (member slot-id tableau)
       (not (empty-slot? slot-id))
       (try-to-foundations slot-id foundation)))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (count-cards slots acc)
  (if (null? slots)
      acc
      (count-cards (cdr slots) (+ acc (length (get-cards (car slots)))))))

(define (calculate-score)
  (set-score! (count-cards foundation 0)))

(define (game-won)
  (= (calculate-score) 52))

(define (hint-slot-to-foundation from-slot to-slots)
  (cond ((null? to-slots) #f)
        ((droppable? from-slot (list (get-top-card from-slot)) (car to-slots))
         (hint-move from-slot 1 (car to-slots)))
        (else (hint-slot-to-foundation from-slot (cdr to-slots)))))

(define (hint-to-foundation from-slots to-slots)
  (cond ((null? from-slots) #f)
        ((empty-slot? (car from-slots))
         (hint-to-foundation (cdr from-slots) to-slots))
        (else (or (hint-slot-to-foundation (car from-slots) to-slots)
                  (hint-to-foundation (cdr from-slots) to-slots)))))

(define (hint-slot-to-tableau from-slot to-slots)
  (cond ((null? to-slots) #f)
        ((empty-slot? (car to-slots)) (hint-slot-to-tableau from-slot (cdr to-slots)))
        ((droppable? from-slot (list (get-top-card from-slot)) (car to-slots))
         (hint-move from-slot 1 (car to-slots)))
        (else (hint-slot-to-tableau from-slot (cdr to-slots)))))

(define (hint-within-tableau from-slots to-slots)
  (cond ((null? from-slots) #f)
        ((or (< (length (get-cards (car from-slots))) 2)
             (let ((card1 (get-top-card (car from-slots)))
                   (card2 (cadr (get-cards (car from-slots)))))
                  (and (suit-eq? card1 card2)
                       (value-offset? 1 card1 card2))))
         (hint-within-tableau (cdr from-slots) to-slots))
        (else (or (hint-slot-to-tableau (car from-slots) to-slots)
                  (hint-within-tableau (cdr from-slots) to-slots)))))

(define (hint-empty-hole from-slots to-slots)
  (cond ((null? from-slots) #f)
        ((not (= (length (get-cards (car from-slots))) 1))
         (hint-empty-hole (cdr from-slots) to-slots))
        (else (or (hint-slot-to-tableau (car from-slots) to-slots)
                  (hint-empty-hole (cdr from-slots) to-slots)))))

; Last resort hint: Find any possible tableau move, even unpleasant ones that were skipped earlier.
(define (hint-last-resort from-slots to-slots)
  (if (null? from-slots)
      #f
      (or (and (not (empty-slot? (car from-slots)))
               (hint-slot-to-tableau (car from-slots) to-slots))
          (hint-last-resort (cdr from-slots) to-slots))))

(define (get-hint)
  (or (hint-to-foundation tableau foundation)
      (hint-empty-hole hole tableau)
      (hint-within-tableau tableau tableau)
      (and (any-slot-empty? hole)
           (list 0 (G_"Move something onto an empty right-hand tableau slot")))
      (hint-last-resort tableau tableau)))

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
