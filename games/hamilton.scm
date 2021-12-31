; AisleRiot - hamilton.scm
; Copyright (C) 1999, 2011, 2014 Timothy Goodwin <toby@flare.email>
; hamilton.scm is based on klondike.scm, which is
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
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

;; For append-map, drop-right, every, find, last:
(use-modules (srfi srfi-1))

;; Setup

(define stock 0)
(define chooser 1)
(define foundation '(2 3 4 5))
(define tableau '(6 7 8 9 10 11 12))

(def-save-var choices 0)
(def-save-var start-value 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)
  (add-normal-slot '() 'chooser)
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

  (deal-tableau tableau)

  (set! choices 3)
  (set! start-value 0)

  (give-status-message) ; this isn't actually displayed at the moment

  (list 7 3.25) ; width and height of playing area
  )

(define (deal-tableau tableau)
  (if (not (null? tableau))
    (begin
      (deal-cards-face-up stock tableau)
      (deal-tableau (cdr tableau)))))

;; Status messages

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
                                        "   "
                                        (get-start-value-string))))

(define (get-value-name value)
  (cond ((eq? value ace) (G_"ace"))
        ((eq? value 2) (G_"two"))
        ((eq? value 3) (G_"three"))
        ((eq? value 4) (G_"four"))
        ((eq? value 5) (G_"five"))
        ((eq? value 6) (G_"six"))
        ((eq? value 7) (G_"seven"))
        ((eq? value 8) (G_"eight"))
        ((eq? value 9) (G_"nine"))
        ((eq? value 10) (G_"ten"))
        ((eq? value jack) (G_"jack"))
        ((eq? value queen) (G_"queen"))
        ((eq? value king) (G_"king"))
        (#t (G_"Unknown value"))))

(define (get-start-value-string)
  (if
    (> start-value 0)
    (string-append (G_"Start card:") " " (get-value-name start-value))
    (string-append (G_"Choices left:") " " (number->string choices))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
                 (number->string (length (get-cards 0)))))

;; Interactions

(define (button-pressed start-slot card-list)
  (cond ((= start-slot stock) #f)
        ((= 0 start-value) (= start-slot chooser))
        (else (valid-list? card-list))))

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
    (complete-transaction start-slot card-list end-slot)
    #f))

(define (button-clicked start-slot)
  (cond ((not (= start-slot stock)) #f)
        ((> choices 0) (choose-next))
        ((= 0 start-value) #f)
        ((not (empty-slot? stock)) (do-deal-next-cards))
        (else #f)))

; find the longest prefix of xs which satisfies the predicate p (p is applied
; to the entire list, not to individual elements); return a pair, first element
; is the prefix, second element is the remainder of the list
(define (split-with-list-pred p xs)
  (define (helper a b)
    (cond ((null? a) (cons a b))
          ((p a) (cons a b))
          (else (helper (drop-right a 1) (append (take-right a 1) b)))))
  (helper xs '()))

(define (button-double-clicked start-slot)
  (cond
    ((= start-slot stock) #f) ; cannot happen - actually deals twice
    ((empty-slot? start-slot) #f)
    ((member start-slot foundation) #f)
    ((= start-slot chooser)
     (complete-transaction chooser
                           (list (remove-card chooser))
                           (car foundation)))
    ((= 0 start-value) #f)
    ((let ((end-slot (find-foundation-for (get-top-card start-slot))))
       (if end-slot
         (let ((split (split-with-list-pred valid-list?
                                            (get-cards start-slot))))
           (set-cards! start-slot (cdr split))
           (complete-transaction start-slot (car split) end-slot))
         #f)))
    (else #f)))

;; Rules

(define (choose-next)
  (set! choices (- choices 1))
  (if (not (empty-slot? chooser))
    (set-cards! stock (append (get-cards stock)
                              (list (flip-card (remove-card chooser))))))
  (flip-stock stock chooser 2 1)
  #t)

(define (dealable?)
  (and
    (not (= 0 start-value))
    (> (length (get-cards stock)) 0)))

(define (do-deal-next-cards)
  (deal-cards-face-up stock
                      (if (= (length (get-cards stock)) 2)
                        (list-head tableau 2)
                        tableau))
  #t)

(define (find-foundation-for card)
  (let ((value (get-value card))
        (suit (get-suit card)))
    (if (= start-value value)
      (find empty-slot? foundation)
      (let ((found (find (lambda (f)
                           (and (not (empty-slot? f))
                                (= suit (get-suit (get-top-card f)))))
                         foundation)))
        (if (and found
                 (value-ok? value (get-value (get-top-card found))))
          found
          #f)))))

(define (complete-transaction start-slot card-list end-slot)
  (add-cards! end-slot
              (if (member end-slot foundation)
                (reverse card-list) card-list))
  (if (member start-slot foundation)
    (add-to-score! -1)) ; can't move more than one off
  (if (member end-slot foundation)
    (begin
      (add-to-score! (length card-list))
      (if (= start-value 0)
        (begin
          (set! choices 0)
          (set! start-value (get-value (car card-list)))))))
  #t)

(define (value-ok? x y)
  (and
    (not (= start-value x))
    (or
      (= x (+ y 1))
      (and (= x ace) (= y king)))))

(define (in-sequence? l)
  (or
    (= (length l) 1)
    (and
      (value-ok? (cadr l) (car l))
      (in-sequence? (cdr l)))))

(define (valid-list? lyst)
  (let ((suit (get-suit (car lyst))))
    (and
      (every (lambda (c) (= suit (get-suit c))) lyst)
      (in-sequence? (map get-value lyst)))))

(define (colour-match? a b)
  (and (eq? (is-red? a) (is-red? b))
       (value-ok? (get-value a) (get-value b))))

(define (suit-match? a b)
  (and (eq? (get-suit a) (get-suit b))
       (value-ok? (get-value a) (get-value b))))

(define (droppable? start-slot card-list end-slot)
  (cond
    ((member end-slot (list start-slot stock chooser)) #f)
    ((member end-slot tableau)
     (and (> start-value 0)
          (or (empty-slot? end-slot)
              (colour-match? (get-top-card end-slot) (last card-list)))))
    ; at this point, end-slot must be a member of foundation
    ((= start-value 0) (= start-slot chooser))
    ((empty-slot? end-slot) (= start-value (get-value (car card-list))))
    (else (suit-match? (car card-list) (get-top-card end-slot)))))

;; Hints

; These hints are simple-minded: they suggest possible moves, but don't
; look ahead to find winning moves. They don't even attempt to find
; suitable cards to fill empty slots. Having exhausted all suit matches,
; they will recommend any possible colour match. Also, there are
; occasions when a colour match is actually preferable to a suit match.
; However, the "Deal another round" hint is only displayed when there
; are no more moves.

(define (cartesian-product xs ys)
  (append-map (lambda (x) (map (lambda (y) (cons x y)) ys)) xs))

; all tableau-foundation pairs
(define t-f-pairs (cartesian-product tableau foundation))

; all tableau-tableau pairs
(define t-t-pairs
  (filter (lambda (x) (not (= (car x) (cdr x))))
          (cartesian-product tableau tableau)))

(define card #f)
(define color 0)
(define suit 0)
(define value 0)
(define slot-id1 0)

(define (not-chosen)
  (and
    (= start-value 0)
    (if (= choices 3)
        (hint-click chooser (G_"Turn over the top card of the stock."))
        (hint-move chooser 1 (car foundation)))))

(define (valid-move? start end)
  (and
    (not (empty-slot? start))
    (droppable? start (list (get-top-card start)) end)))

; Given a slot, return the longest moveable sequence of cards in it
(define (get-moveable slot)
  (car (split-with-list-pred valid-list? (get-cards slot))))

; Given a pair of slots, start and end, hint if there is a valid move from
; start to end. If the end slot is a foundation, the hint is just the first
; card. If in the tableau, the hint must be the longest moveable list of cards.
(define (maybe-hint p)
  (letrec ((start (car p))
           (end (cdr p))
           (cards (if (member end foundation)
		      (list (get-top-card start))
                      (get-moveable start))))
    (and
      (not (empty-slot? start))
      (droppable? start cards end)
      (hint-move start (length cards) end))))

(define (hint-foundation)
  (or-map maybe-hint t-f-pairs))

(define (maybe-suit p)
  (and
    (not (empty-slot? (car p)))
    (not (empty-slot? (cdr p)))
    (= (get-suit (get-top-card (car p)))
       (get-suit (get-top-card (cdr p))))
    (maybe-hint p)))

(define (get-hint)
  (or
    (not-chosen)
    ; Move to foundation?
    (or-map maybe-hint t-f-pairs)
    ; Match within suit?
    (or-map maybe-suit t-t-pairs)
    ; Empty slot?
    (and
      (or-map empty-slot? tableau)
      (list 0 (G_"Fill an empty slot.")))
    ; Colour matches are the last resort...
    (or-map maybe-hint t-t-pairs)
    ; ... apart from dealing, of course.
    (and
      (not (empty-slot? stock))
      (hint-click stock (G_"Deal a new round.")))
    ; If all else fails.
    (list 0 (G_"Try moving cards down from the foundations."))))

(define (game-won) (= (get-score) 52))

; We never say "game over".
(define (game-over)
  (give-status-message)
  (not (game-won)))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features dealable-feature droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable? dealable? do-deal-next-cards)

; This game is sometimes called Agnes, but I call it Hamilton after my
; father, Geoffrey Hamilton Goodwin (b. 1937), who taught me the game
; many many years ago. #t
