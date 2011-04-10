; AisleRiot - ten-across.scm
;; base on klondike.scm
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
; Copyright (C) 1999 James LewisMoss <dres@debian.org>
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

(define allow-two-spot-use #t)

; The set up:

(define tableau '(2 3 4 5 6 7 8 9 10 11))
(define tmp-spots '(0 1))
(define stock 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  
  (make-standard-deck)
  (shuffle-deck)
  
  (add-blank-slot)
  (add-normal-slot DECK)
  (add-normal-slot '())
  (add-carriage-return-slot)
  (map (lambda (ignore) (add-extended-slot '() down)) tableau)
  (map (lambda (slot)
         (set-slot-y-expansion!
          slot 0.25))
       tableau)
  (deal-ten-across-cards)

  (deal-cards-face-up stock '(1))

  (flip-top-card stock)
  
  (list 10 4))

(define (deal-ten-across-cards)
  (let* ((deal-len (length tableau))
         (direction #t)
         (deal-ten-across-int
          (lambda (num)
            (if direction
                (begin
                  (deal-cards-face-up stock (list-head tableau num))
                  (deal-cards stock
                              (list-head
                               (list-tail tableau num) (- deal-len num num)))
                  (deal-cards-face-up stock (list-tail tableau (- deal-len num))))
                (begin
                  (deal-cards-face-up stock
                                      (reverse (list-tail tableau
                                                          (- deal-len num))))
                  (deal-cards stock
                              (reverse (list-head
                                        (list-tail tableau num)
                                        (- deal-len num num))))
                  (deal-cards-face-up stock (reverse (list-head tableau num)))))
            (set! direction (not direction)))))
  (map (lambda (num-now) (deal-ten-across-int num-now)) '(1 2 3 4 5))))

;; testing functions
;;(define deal-cards (lambda (num slot-list) (map (lambda (num1) (display "dealing face-down to ")(display num1)(display "\n")) slot-list)))
;;(define deal-cards-face-up (lambda (num slot-list) (map (lambda (num1) (display "dealing face-up to ") (display num1) (display "\n")) slot-list)))
;;(deal-ten-across-cards)

(define (button-pressed slot-id card-list)
  (and (or (> slot-id 1)
           (and (member slot-id tmp-spots)
                (= (length card-list) 1)))
       (is-visible? (car (reverse card-list)))))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (and (not (empty-slot? start-slot)) 
           (member start-slot tableau))
      (make-visible-top-card start-slot))
  #t)

(define (is-ok-to-place card1 card2)
  (and (= (get-suit card1)
          (get-suit card2))
       (= (get-value card2)
          (+ (get-value card1) 1))))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (member end-slot tableau)
                (if (empty-slot? end-slot)
                    (= king (get-value (car (reverse card-list))))
                    (is-ok-to-place (car (reverse card-list))
                                    (get-top-card end-slot))))
           (and allow-two-spot-use
                (member end-slot tmp-spots)
                (= 1 (length card-list))
                (empty-slot? end-slot)))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (button-clicked start-slot)
  #f)

(define (button-double-clicked start-slot)
  ;; uncomment for some debugging output :)
;;  (display (get-cards 0))
;;  (newline)
;;  (display (get-cards 1))
;;  (newline)
;;  (display (get-cards 2))
;;  (newline)
;;  (display (get-cards 3))
;;  (newline)
;;  (display (get-cards 4))
;;  (newline)
;;  (display (get-cards 5))
;;  (newline)
;;  (display (get-cards 6))
;;  (newline)
;;  (display (get-cards 7))
;;  (newline)
;;  (display (get-cards 8))
;;  (newline)
;;  (display (get-cards 9))
;;  (newline)
;;  (display (get-cards 10))
;;  (newline)
;;  (display (get-cards 11))
;;  (newline)
  #f)

;; three things to test for
;; 1) empty slot and a king not currently in an empty slot
;; 2) a visible card that will fit on the end of a current row
;; 3) a single card at the top of a stack either of non-visible cards
;;    or non-connected cards and an empty temporary spot.

;;----------------------------------------------------------------------
(define (have-empty-slot? slot-list)
  (or-map (lambda (item) (= 0 (length (get-cards item)))) slot-list))

(define (king? card)
  (= (get-value card) king))

(define (get-good-king-for-empty-move slot-list)
  (or-map (lambda (item)
            (let ((cards1 (get-cards item)))
                   ;; cut out the last card because if it's a king we
                   ;; don't want to move it
              (if (> (length cards1) 0)
                  (or-map (lambda (item) (if (and (is-visible? item)
                                              (king? item))
                                             item
                                         #f))
                      (list-head cards1 (- (length cards1) 1)))
                  #f)))
          slot-list))

;; ** 3 **
(define (test-king-move slot-list)
  (if (have-empty-slot? slot-list)
      (let ((good-king (get-good-king-for-empty-move slot-list)))
        (if (list? good-king)
            (list 2 (get-name good-king) (_"an empty slot"))
            #f))
      #f))

;;----------------------------------------------------------------------
(define (find-card-for item slot-num slot-list)
  (or-map (lambda (slot)
            (or-map (lambda (card)
                      (if (and (not (= slot-num slot))
                               (is-visible? card)
                               (is-ok-to-place card item))
                          (list card item)
                          #f))
                      (get-cards slot)))
          slot-list))

;; ** 2 **
(define (test-stack-move slot-list tmp-list)
  (let ((cards (or-map
                (lambda (slot)
                  (let ((card-list (get-cards slot)))
                    (if (not (null? card-list))
                        (find-card-for (car card-list) slot slot-list)
                        #f)))
                slot-list)))
        (if (list? cards)
            (list 2 (get-name (car cards)) (get-name (cadr cards)))
            #f)))

;;----------------------------------------------------------------------
(define (get-top-cards slot-list)
  (map (lambda (slot)
         (let ((cards (get-cards slot)))
           (if (null? cards)
               '()
               (car cards))))
       slot-list))

;; ** 1 **
(define (test-for-tmp-move-down slot-list tmp-list)
  (let* ((move-to-cards (get-top-cards slot-list))
         (move-from-cards (get-top-cards tmp-list))
         (cards (or-map (lambda (card1)
                          (or-map (lambda (card2)
                                    (cond ((and (null? card2)
                                                (not (null? card1))
                                                (king? card1))
                                           (list card1 (_"an empty slot")))
                                          ((and (not (null? card1))
                                                (not (null? card2))
                                                (is-ok-to-place card1 card2))
                                           (list card1 card2))
                                          (#t #f)))
                                  move-to-cards))
                        move-from-cards)))
    (if (list? cards)
        (list 1
              (get-name (car cards))
              (if (list? (cadr cards))
                  (get-name (cadr cards))
                  (cadr cards)))
        #f)))

;;----------------------------------------------------------------------
(define (get-hint)
  (or
   (test-for-tmp-move-down tableau tmp-spots)
   (test-stack-move tableau tmp-spots) 
   (test-king-move tableau) 
   (and allow-two-spot-use
        (have-empty-slot? tmp-spots)
        (list 0 (_"Move a card to an empty temporary slot")))
   (list 0 (_"No hint available"))))

(define final-stack-helper
  (lambda (the-list num suit)
    (if (null? the-list)
        #t
        (let ((card (car the-list))
              (rest (cdr the-list)))
          (if (and (is-visible? card)
                   (= suit (get-suit card))
                   (= num (get-value card)))
              (final-stack-helper rest (+ 1 num) suit)
              #f)))))

(define (final-stack? card-list)
    (final-stack-helper card-list 1 (get-suit (car card-list))))

(define won-tester
  (lambda (slot-list)
    (let ((to-test (car slot-list))
          (to-cont (cdr slot-list)))
      (if (or (and (= 13 (length (get-cards to-test)))
                   (final-stack? (get-cards to-test)))
              (= 0 (length (get-cards to-test))))
          (if (equal? to-cont '())
              #t
              (won-tester to-cont))
          #f))))
  
(define (game-won)
  (won-tester tableau))

(define (game-over)
  (not (game-won)))

(define (get-options)
  (list (list (_"Allow temporary spots use") allow-two-spot-use)))

(define (apply-options options)
  (set! allow-two-spot-use (cadar options)))

(define (timeout) #f)

(set-features droppable-feature scores-disabled)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
