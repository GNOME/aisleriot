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

(use-modules (aisleriot interface) (aisleriot api))

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

;;----------------------------------------------------------------------
(define (have-empty-slot? slot-list)
  (or-map (lambda (item) (= 0 (length (get-cards item)))) slot-list))

(define (king? card)
  (= (get-value card) king))

(define (find-king-move slot card-list count to-slot)
  (cond ((null? card-list)
         #f)
        ((and (= (length card-list) 1)
              (= (get-value (car card-list)) king)
              (member slot tableau))
         ; Top card in this tableau pile is a king; don't bother moving it to another empty slot.
         #f)
        ((or (not (is-visible? (car card-list)))
             (not (= (get-value (car card-list)) king)))
         (find-king-move slot (cdr card-list) (+ count 1) to-slot))
        (#t
         (hint-move slot count to-slot))))

;;----------------------------------------------------------------------
(define (find-placeable-card card-list dest-card count)
  (cond ((null? card-list)
         #f)
        ((not (is-visible? (car card-list)))
         #f)
        ((is-ok-to-place (car card-list) dest-card)
         count)
        (#t
         (find-placeable-card (cdr card-list) dest-card (+ count 1)))))

(define (find-stack-move from-slot slot-list)
  (cond ((null? slot-list)
         #f)
        ((= from-slot (car slot-list))
         (find-stack-move from-slot (cdr slot-list)))
        ((empty-slot? (car slot-list))
         (or (find-king-move from-slot (get-cards from-slot) 1 (car slot-list))
             (find-stack-move from-slot (cdr slot-list))))
        ((find-placeable-card (get-cards from-slot) (get-top-card (car slot-list)) 1)
         (hint-move from-slot (find-placeable-card (get-cards from-slot) (get-top-card (car slot-list)) 1) (car slot-list)))
        (#t
         (find-stack-move from-slot (cdr slot-list)))))

(define (test-stack-move slot-list)
  (if (null? slot-list)
      #f
      (or (find-stack-move (car slot-list) tableau)
          (test-stack-move (cdr slot-list)))))

;;----------------------------------------------------------------------
(define (get-top-cards slot-list)
  (map (lambda (slot)
         (let ((cards (get-cards slot)))
           (if (null? cards)
               '()
               (car cards))))
       slot-list))

;;----------------------------------------------------------------------
(define (get-hint)
  (or
   (test-stack-move (append tmp-spots tableau))
   (and allow-two-spot-use
        (have-empty-slot? tmp-spots)
        (list 0 (G_"Move a card to an empty temporary slot")))
   (list 0 (G_"No hint available"))))

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
  (list (list (G_"Allow temporary spots use") allow-two-spot-use)))

(define (apply-options options)
  (set! allow-two-spot-use (cadar options)))

(define (timeout) #f)

(set-features droppable-feature scores-disabled)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
