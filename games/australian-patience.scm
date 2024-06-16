; AisleRiot - australian-patience.scm
; Copyright (C) 2022, 2023 Gwyneth Morgan
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
(define waste 1)
(define foundation '(2 3 4 5))
(define tableau '(6 7 8 9 10 11 12))

(define (foundation? slot)
  (and (>= slot 2) (<= slot 5)))

(define (tableau? slot)
  (and (>= slot 6) (<= slot 12)))

(define (new-game)
  (initialize-playing-area)

  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)
  (add-normal-slot '() 'waste)
  (add-blank-slot)
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

  (deal-cards-face-up stock (append tableau tableau tableau tableau))

  (list 7 4.1))

(define (button-pressed slot-id card-list)
  (or (tableau? slot-id)
      (= slot-id waste)))

(define (droppable? start-slot card-list end-slot)
  (cond ((and (foundation? end-slot)
              (null? (cdr card-list)))
         (if (empty-slot? end-slot)
           (eq? (get-value (car card-list)) ace)
           (and (suit-eq? (car card-list) (car (get-cards end-slot)))
                (eq? (get-value (car card-list))
                     (+ (get-value (car (get-cards end-slot))) 1)))))
        ((tableau? end-slot)
         (if (empty-slot? end-slot)
           (eq? (get-value (car (reverse card-list))) king)
           (and (suit-eq? (car (reverse card-list))
                          (car (get-cards end-slot)))
                (eq? (+ (get-value (car (reverse card-list))) 1)
                     (get-value (car (get-cards end-slot)))))))
        (else #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-cards start-slot card-list end-slot)))

(define (move-cards start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (begin
    (and (foundation? end-slot)
         (add-to-score! 1))
    (autoplay-foundations)))

(define (autoplay-foundations)
  (let tableau-loop ((tableau-slots (append (list waste) tableau)))
    (let foundation-loop ((foundation-slots foundation))
      (let* ((tableau-slot (car tableau-slots))
             (foundation-slot (car foundation-slots))
             (cards (get-cards tableau-slot)))
        (if (and
              (not (nil? cards))
              (droppable? tableau-slot
                          (list (car cards))
                          foundation-slot))
          (move-cards
            tableau-slot
            (list (remove-card tableau-slot))
            foundation-slot)
          (if (nil? (cdr foundation-slots))
            (if (nil? (cdr tableau-slots))
              #t
              (tableau-loop (cdr tableau-slots)))
            (foundation-loop (cdr foundation-slots))))))))

(define (button-clicked slot-id)
  (and (= slot-id stock)
       (flip-stock stock waste 0)))

(define (button-double-clicked slot-id)
  (autoplay-foundations))

(define (game-continuable)
  (not (game-won)))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)))

(define (get-hint)
  #f)

(define (get-options)
  #f)

(define (apply-options options)
  #f)

(define (timeout)
  #f)

(define (dealable?)
  (flippable? stock waste 0))

(define (do-deal-next-cards)
  (flip-stock stock waste 1))

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked
            button-double-clicked game-continuable game-won get-hint
            get-options apply-options timeout droppable? dealable?)
