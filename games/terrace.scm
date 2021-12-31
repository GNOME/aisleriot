; AisleRiot - terrace.scm
; Copyright (C) 2008 Vincent Povirk <madewokherd@gmail.com>
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

(define reserve-size 11)
(define tableau-size 9)
(define build-foundation-in-suit #f)
(define select-base #t)
(define max-redeal 0)
(define auto-fill-tableau #f)
(define fill-from-reserve #f)

(define variations
  '((11 9 #f #t 0 #f #f)
    (13 9 #t #t 1 #f #f)
    (11 9 #f #f 0 #f #f)
    (11 9 #f #f 0 #t #f)
    (21 8 #f #f 0 #t #t)
    (10 8 #f #f 0 #f #f)
    (10 9 #f #f 0 #f #f)))

(define variation-names
  (list
        ; Translators: this string is the name of a variant of this game. If there is an established standard name for this game or game variant in your locale, use that; otherwise you can translate this string freely or literally, at your option.
        (G_"Terrace")
        ; Translators: this string is the name of a variant of this game. If there is an established standard name for this game or game variant in your locale, use that; otherwise you can translate this string freely or literally, at your option.
        (G_"General's Patience")
        ; Translators: this string is the name of a variant of this game. If there is an established standard name for this game or game variant in your locale, use that; otherwise you can translate this string freely or literally, at your option.
        (G_"Falling Stars")
        ; Translators: this string is the name of a variant of this game. If there is an established standard name for this game or game variant in your locale, use that; otherwise you can translate this string freely or literally, at your option.
        (G_"Signora")
        ; Translators: this string is the name of a variant of this game. If there is an established standard name for this game or game variant in your locale, use that; otherwise you can translate this string freely or literally, at your option.
        (G_"Redheads")
        ; Translators: this string is the name of a variant of this game. If there is an established standard name for this game or game variant in your locale, use that; otherwise you can translate this string freely or literally, at your option.
        (G_"Blondes and Brunettes")
        ; Translators: this string is the name of a variant of this game. If there is an established standard name for this game or game variant in your locale, use that; otherwise you can translate this string freely or literally, at your option.
        (G_"Wood")))

(define current-variation 0)

(def-save-var BASE-VAL 0)

; In games where the tableau is not auto-filled, the stock is "locked" when a
; card is dealt while the tableau has an empty space and "unlocked" when spaces
; are full or a card is moved out of the waste.
(def-save-var stock-locked #f)

(define stock 0)
(define waste 1)
(define reserve 2)
(define foundation '(3 4 5 6 7 8 9 10))
(define tableau '()) ; This will be set by (new-game)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-double-deck)
  (shuffle-deck)
  
  (add-normal-slot (reverse DECK) 'stock)
  (add-normal-slot '() 'waste)
  (add-blank-slot)
  (add-extended-slot '() right 'reserve)
  (add-carriage-return-slot)

  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (set! tableau '())
  (build-tableau-slots tableau-size)
  (set! tableau (reverse tableau))

  (deal-reserve-cards reserve-size)

  (if select-base
      (begin (deal-tableau-cards tableau 4)
             (set! BASE-VAL 0))
      (begin (deal-cards-face-up stock (list (car foundation)))
             (set! BASE-VAL (get-value (get-top-card (car foundation))))
             (deal-tableau-cards tableau tableau-size)))

  (do-auto-deal)
  (give-status-message)
  (calculate-score)

  (list 8 4.1)
)

(define (build-tableau-slots count)
  (and (not (= count 0))
       (set! tableau (cons SLOTS tableau))
       (add-extended-slot '() down 'tableau)
       (set! HORIZPOS (+ HORIZPOS (- 1 (/ tableau-size 8))))
       (build-tableau-slots (- count 1))))

(define (deal-reserve-cards count)
  (and (not (= count 0))
       (deal-cards-face-up stock (list reserve))
       (deal-reserve-cards (- count 1))))

(define (deal-tableau-cards slots count)
  (and (not (= count 0))
       (not (null? slots))
       (deal-cards-face-up stock (list (car slots)))
       (deal-tableau-cards (cdr slots) (- count 1))))


(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
                                        (get-redeals-string)
                                        "   "
                                        (get-base-string))))

(define (get-base-string)
  (cond ((and (> BASE-VAL 1)
              (< BASE-VAL 11))
         (format #f (G_"Base Card: ~a") (number->string BASE-VAL)))
        ((= BASE-VAL 1)
         (G_"Base Card: Ace"))
        ((= BASE-VAL 11)
         (G_"Base Card: Jack"))
        ((= BASE-VAL 12)
         (G_"Base Card: Queen"))
        ((= BASE-VAL 13)
         (G_"Base Card: King"))
        (#t "")))

(define (get-redeals-string)
  (if (or (< max-redeal 1) (= BASE-VAL 0))
      ""
      (string-append "   " (G_"Redeals left:") " "
                     (number->string (- max-redeal FLIP-COUNTER)))))

(define (get-stock-no-string)
  (if (= BASE-VAL 0)
      ""
      (string-append (G_"Stock left:") " " 
                     (number->string (length (get-cards stock))))))

(define (descending-values? a b)
   (or (= b (- a 1))
       (and (= a ace)
            (= b king))))

(define (calculate-score-helper slots acc)
  (if (null? slots)
      acc
      (calculate-score-helper (cdr slots) (+ acc (length (get-cards (car slots)))))))

(define (calculate-score)
  (set-score! (calculate-score-helper foundation 0)))

(define (do-auto-fill-tableau slots)
  (if (null? slots)
      #t
      (begin (and (empty-slot? (car slots))
                  (cond ((and fill-from-reserve (not (empty-slot? reserve)))
                         (deal-cards-face-up reserve (list (car slots))))
                        ((not (empty-slot? waste))
                         (deal-cards-face-up waste (list (car slots))))
                        ((not (empty-slot? stock))
                         (deal-cards-face-up stock (list (car slots))))))
             (do-auto-fill-tableau (cdr slots)))))

(define (do-auto-deal)
  (or (= BASE-VAL 0)
      (not auto-fill-tableau)
      (do-auto-fill-tableau tableau))
  (or (= BASE-VAL 0)
      (not (empty-slot? waste))
      (not (dealable?))
      (do-deal-next-cards))
  #t)

(define (button-pressed slot-id card-list)
  (cond ((= BASE-VAL 0) ; If we haven't selected a base, nothing else is allowed
         (member slot-id tableau))
        ((member slot-id tableau)
         (= 1 (length card-list)))
        ((= slot-id reserve)
         #t)
        ((= slot-id waste)
         #t)
        (#t #f)))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (or (not (= BASE-VAL 0))
      (begin (set! BASE-VAL (get-value (get-top-card end-slot)))
             (do-auto-fill-tableau tableau)))
  (and (or (= start-slot waste)
           (not (or-map empty-slot? tableau)))
       (set! stock-locked #f))
  (do-auto-deal)
  #t)

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (droppable? start-slot card-list end-slot)
  (cond ((= BASE-VAL 0) ; If we haven't selected a base, nothing else is allowed
         (and (member start-slot tableau)
              (member end-slot foundation)))
        ((= start-slot stock)
         #f)
        ((member end-slot tableau)
         (and (= 1 (length card-list))
              (not (= start-slot end-slot))
              (not (= start-slot reserve))
              (if (empty-slot? end-slot)
                  (= start-slot waste)
                  (and (not (= (get-color (car card-list))
                               (get-color (get-top-card end-slot))))
                       (descending-values? (get-value (get-top-card end-slot))
                                           (get-value (car card-list)))
                       (not (= (get-value (get-top-card end-slot)) BASE-VAL))))))
        ((member end-slot foundation)
         (if (empty-slot? end-slot)
             (= BASE-VAL (get-value (car card-list)))
             (and (if build-foundation-in-suit
                      (= (get-suit (car card-list))
                         (get-suit (get-top-card end-slot)))
                      (not (= (get-color (car card-list))
                              (get-color (get-top-card end-slot)))))
                  (descending-values? (get-value (car card-list))
                                      (get-value (get-top-card end-slot)))
                  (not (= (get-value (car card-list)) BASE-VAL)))))
        (#t #f)))

(define (dealable?)
  (and (not (= 0 BASE-VAL))
       (flippable? stock waste max-redeal)
       ;Do not allow deals if we've been through the deck once and the waste is not empty
       (or (< FLIP-COUNTER 1)
           (empty-slot? waste))
       (or auto-fill-tableau
           (not stock-locked))))

(define (do-deal-next-cards)
  (and (dealable?)
       (flip-stock stock waste max-redeal 1)
       (or (not (or-map empty-slot? tableau))
           (set! stock-locked #t))
       #t))

(define (button-clicked start-slot)
  (and (= start-slot stock)
       (do-deal-next-cards)
       #t))

(define (auto-play-to-foundation start-slot end-slots)
  (and (not (null? end-slots))
       (not (empty-slot? start-slot))
       (if (droppable? start-slot (list (get-top-card start-slot)) (car end-slots))
           (complete-transaction start-slot (list (remove-card start-slot)) (car end-slots))
           (auto-play-to-foundation start-slot (cdr end-slots)))))

(define (button-double-clicked start-slot)
  (auto-play-to-foundation start-slot foundation))

(define (hint-start-foundation)
  (and (= BASE-VAL 0)
       (list 2 (G_"something") (G_"the foundation"))))

(define (hint-slot-to-foundation start-slot end-slots)
  (and (not (null? end-slots))
       (not (empty-slot? start-slot))
       (if (droppable? start-slot (list (get-top-card start-slot)) (car end-slots))
           (hint-move start-slot 1 (car end-slots))
           (hint-slot-to-foundation start-slot (cdr end-slots)))))

(define (hint-slots-to-foundation start-slots)
  (and (not (null? start-slots))
       (or (hint-slot-to-foundation (car start-slots) foundation)
           (hint-slots-to-foundation (cdr start-slots)))))

(define (get-rank value)
  (if (< value BASE-VAL)
      (+ 13 value)
      value))

(define (droppable-on-foundation start-slot cards end-slots)
  (and (not (null? end-slots))
       (or (droppable? start-slot cards (car end-slots))
           (droppable-on-foundation start-slot cards (cdr end-slots)))))

; We need to check recursively for builds because it might be possible to free
; a space in the tableau by moving multiple single cards in a row.
(define (buildable-on-tableau start-slot num-cards cards acc)
  (or (and (null? cards)
           acc)
      ; If the foundation is building in suit, it's possible that moving cards
      ; within the tableau will allow putting something on a foundation
      (and (droppable-on-foundation start-slot (list (car cards)) foundation)
           acc)
      (let ((target-slot (buildable-on-tableau-helper start-slot (car cards) tableau)))
           (and target-slot
                (buildable-on-tableau
                     start-slot
                     (+ num-cards 1)
                     (cdr cards)
                     (or acc (cons (get-rank (get-value (car cards))) (hint-move start-slot num-cards target-slot))))))))
(define (buildable-on-tableau-helper start-slot card end-slots)
  (and (not (null? end-slots))
       (or (and (not (empty-slot? (car end-slots)))
                (droppable? start-slot (list card) (car end-slots))
                (car end-slots))
           (buildable-on-tableau-helper start-slot card (cdr end-slots)))))

(define (hint-tableau-build start-slots acc)
  (if (null? start-slots)
      (cdr acc)
      ; Try to prefer moving cards of higher rank
      (let ((hint (hint-tableau-build-helper (car start-slots))))
           (hint-tableau-build
               (cdr start-slots)
               (if (and hint (> (car hint) (car acc)))
                   hint
                   acc)))))
             
            
(define (hint-tableau-build-helper start-slot)
  (and (not (empty-slot? start-slot))
       (buildable-on-tableau start-slot 1 (get-cards start-slot) #f)))

(define (hint-waste-to-tableau end-slots)
  (and (not (null? end-slots))
       (not (empty-slot? waste))
       (if (droppable? waste (list (get-top-card waste)) (car end-slots))
           (hint-move waste 1 (car end-slots))
           (hint-waste-to-tableau (cdr end-slots)))))

(define (hint-deal)
  (and (dealable?)
       (list 0 (G_"Deal a new card from the deck"))))

(define (get-hint)
  (or (hint-start-foundation)
      (hint-slot-to-foundation reserve foundation)
      (hint-slots-to-foundation tableau)
      (hint-slot-to-foundation waste foundation)
      (hint-tableau-build tableau (cons BASE-VAL #f))
      (hint-waste-to-tableau tableau)
      (hint-deal)))

(define (game-won)
  (= (get-score) 104))

(define (game-continuable)
  (give-status-message)
  (calculate-score)
  (and (not (game-won))
       (get-hint)))

(define (get-variation-options names index)
  (if (null? names)
      '()
      (cons (list (car names) (= current-variation index))
            (get-variation-options (cdr names) (+ 1 index)))))

(define (get-options)
  (append '(begin-exclusive)
          (get-variation-options variation-names 0)
          '(end-exclusive)))

(define (set-variation vars options index)
  (or (null? vars)
      (and (cadar options)
           (set! current-variation index)
           (set! reserve-size (list-ref (car vars) 0))
           (set! tableau-size (list-ref (car vars) 1))
           (set! build-foundation-in-suit (list-ref (car vars) 2))
           (set! select-base (list-ref (car vars) 3))
           (set! max-redeal (list-ref (car vars) 4))
           (set! auto-fill-tableau (list-ref (car vars) 5))
           (set! fill-from-reserve (list-ref (car vars) 6)))
      (set-variation (cdr vars) (cdr options) (+ index 1))))

(define (apply-options options)
  (set-variation variations (cdr options) 0))

(define (timeout) #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-continuable game-won get-hint get-options apply-options timeout droppable? dealable?)


