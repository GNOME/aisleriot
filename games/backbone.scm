; AisleRiot - backbone.scm
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>,
;                      2005 Vincent Povirk <madewokherd@gmail.com>
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

(define tableau '(0 1 6 7 12 13 16 17))
(define foundation '(2 3 4 5 8 9 10 11))
(define stock 14)
(define waste 15)
(define reserve '(18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36))

(define (reserve? slot) (> slot 17))
(define (playable? slot)
  (or 
    (= slot 17)
    (member slot tableau)
    (reserve? slot)))

;list of lists of slots obscuring a slot
(define obscured '())

(define (padded-list n tail)
  (if (= n 0)
      tail
      (padded-list (- n 1) (cons '() tail))))

(define (make-backbone n)
  (add-normal-slot '() 'reserve)
  (set! HORIZPOS (- HORIZPOS 1))
  (set! VERTPOS (+ VERTPOS (/ 1 3)))
  (set-car! (list-tail obscured n) (list (+ n 1)))
  (if (>= VERTPOS 3)
      n
      (make-backbone (+ n 1))))

(define (deal-cards-face-up-to-reserve-from n)
  (deal-cards-face-up stock (list n))
  (if (>= n 36)
      #t
      (deal-cards-face-up-to-reserve-from (+ n 1))))

(define (deal-cards-face-up-to-reserve)
  (deal-cards-face-up-to-reserve-from 18))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (set! obscured (padded-list 41 '()))

  (make-standard-double-deck)
  (shuffle-deck)
  
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  
  (add-carriage-return-slot)
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  
  (add-carriage-return-slot)
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-normal-slot DECK 'stock)
  (add-normal-slot '() 'waste)

  (add-carriage-return-slot)
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'tableau)

  (deal-cards-face-up stock tableau)

  (set! HORIZPOS 1)
  (set! VERTPOS 0)
  (set-car! (list-tail obscured (make-backbone 18)) (list 36))

  (set! HORIZPOS 2)
  (set! VERTPOS 0)
  (make-backbone 27)
  
  (set! HORIZPOS 1.5)
  (add-normal-slot '() 'reserve)
  
  (deal-cards-face-up-to-reserve)

  (give-status-message)
  
  (list 8 4)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (get-redeals-string)
  (string-append (G_"Redeals left:") " "
		 (number->string (- 1 FLIP-COUNTER))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards stock)))))

(define (empty-slots? slots)
  (if (eq? slots '())
      #t
      (and
         (empty-slot? (car slots))
         (empty-slots? (cdr slots)))))

(define (is-playable? slot)
  (and (or
         (member slot tableau)
         (= slot waste)
         (reserve? slot))
       (empty-slots? (list-ref obscured slot))))

(define (is-legal-move? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (not (eq? card-list '()))
       (is-playable? start-slot)
       (or (and (member end-slot foundation)
                (if (empty-slot? end-slot)
                    (= (get-value (car card-list)) 1)
                    (and 
                      (= (get-suit (car card-list))
                         (get-suit (get-top-card end-slot)))
                      (= (get-value (car card-list))
                         (+ (get-value (get-top-card end-slot)) 1)))))
           (and (member end-slot tableau)
                (if (empty-slot? end-slot)
                    (not (reserve? start-slot))
                    (and
                      (= (get-suit (car card-list))
                         (get-suit (get-top-card end-slot)))
                      (= (get-value (car card-list))
                         (- (get-value (get-top-card end-slot)) 1))))))))

(define (button-pressed slot-id card-list)
  (is-playable? slot-id))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (member start-slot foundation)
      (add-to-score! -1))
  (if (member end-slot foundation)
      (add-to-score! 1))
  #t)

(define (button-released start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (is-legal-move? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (dealable?)
  (flippable? stock waste 1))

(define (do-deal-next-cards)
  (flip-stock stock waste 1 1))

(define (button-clicked start-slot)
  (and (= start-slot stock)
       (flip-stock stock waste 1 1)))

(define (move-if-possible start-slot end-slots)
  (and (not (empty-slot? start-slot))
       (let ((card (get-top-card start-slot)))
         (if (is-legal-move? start-slot (list card) (car end-slots))
             (begin (remove-card start-slot)
                  (complete-transaction start-slot (list card) (car end-slots)))
             (if (eq? (cdr end-slots) '())
                 #f
                 (move-if-possible start-slot (cdr end-slots)))))))

(define (button-double-clicked start-slot)
  (move-if-possible start-slot foundation))

(define (non-empty-piles-helper piles result)
  (if (eq? piles '())
      result
      (non-empty-piles-helper
        (cdr piles)
        (if (empty-slot? (car piles))
            result
            (cons (car piles) result)))))

(define (non-empty-piles piles)
  (non-empty-piles-helper piles '()))

(define (empty-piles piles)
  (define (empty-piles piles result)
    (if (eq? piles '())
        result
        (empty-piles
          (cdr piles)
          (if (empty-slot? (car piles))
              (cons (car piles) result)
              result))))
  (empty-piles piles '()))

(define (get-legal-move-from-source source targets)
  (if (eq? targets '())
      #f
      (if (and (not (empty-slot? source))
               (is-legal-move? source (list (car (get-cards source))) (car targets)))
          (list source (car targets))
          (get-legal-move-from-source source (cdr targets)))))

(define (get-legal-move sources targets)
  (if (eq? sources '())
      #f
      (or (get-legal-move-from-source (car sources) targets)
          (get-legal-move (cdr sources) targets))))

(define (can-send-tableau-card-to-any-slot? card slots)
  (if (eq? slots '())
      #f
      (or (is-legal-move? (car tableau) (list card) (car slots))
          (can-send-tableau-card-to-any-slot? card (cdr slots)))))

(define (useful-tableau-stack? cards)
  (if (eq? cards '())
      #t
      (and (can-send-tableau-card-to-any-slot? (car cards) (non-empty-piles tableau))
           (useful-tableau-stack? (cdr cards)))))

(define (get-move-from-tableau sources)
  (if (eq? sources '())
      #f
      (let ((move (get-legal-move-from-source (car sources) foundation)))
           (if move
               move
               (let ((move (get-legal-move-from-source (car sources) (non-empty-piles tableau))))
                    (if (and move
                             (useful-tableau-stack? (cdr (get-cards (car sources)))))
                        move
                        (get-move-from-tableau (cdr sources))))))))

(define (get-hint)
  (or 
    (let
         ((move (or 
                   (get-legal-move
                     (append reserve (list waste))
                     (append foundation (non-empty-piles tableau) (list waste)))
                   (get-move-from-tableau tableau)
                   (get-legal-move-from-source
                     waste
                     (empty-piles tableau)))))
        (if move
            (hint-move (car move) 1 (cadr move))
            #f))
    (and (or (not (empty-slot? stock))
             (and (< FLIP-COUNTER 1)
                  (not (empty-slot? waste))))
         (list 0 (G_"Deal a new card from the deck")))
    (and (get-legal-move tableau tableau)
         (list 0 (G_"Try rearranging the cards")))))

(define (full-piles? piles)
  (and (= 13 (length (get-cards (car piles))))
       (or (eq? (cdr piles) '())
           (full-piles? (cdr piles)))))

(define (game-won)
  (full-piles? foundation))

(define (game-over)
  (give-status-message)
  (and (not (game-won)))
       (get-hint))

(define (get-options)
  '())

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout is-legal-move? dealable?)
