; AisleRiot - gay_gordons.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
;
; Code to construct a winnable deal: Callum McKenzie with inspriation
; from Jeff Barry. 2005/04/14.
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

; Check if two cards match within the rules of the game.
;
; Note that we can use simple sums for normal matches and
; for Kings and Queens, but we aren't so lucky with Jacks.
(define (is-pair? card1 card2)
  (let* ((val1 (get-value card1))
        (val2 (get-value card2))
        (sum  (+ val1 val2)))
    (or (= 11 sum)        ; Normal match
       (and (= 11 val1)  ; Two Jacks
            (= 11 val2))
       (= 25 sum))))     ; King + Queen

; Return the index of the first suitable pair in the list.
;
; This assumes that there is a suitable matching pair. If cards are taken
; out of a full deck pair-wise this assumption is always true.
; Randomness is assured by the randomness of the original deck.
(define (find-match target list n)
  (cond ((null? target) n) ; This should never happen on good input
       ((is-pair? target (car list)) n)
       (#t (find-match target (cdr list) (+ 1 n)))))

; Rearrange the deck so it is in matched pairs.
;
; This way we can easily lay-out a guaranteed-to-be-solvable game.
(define (pair-deck cards)
  (cond ((null? cards) '())
       ((null? (car cards)) (pair-deck (cdr cards)))
       (#t (let* ((first (car cards))
                  (remainder (cdr cards))
                  (n (find-match first remainder 0))
                  (nth (list-ref remainder n)))
             (list-set! remainder n (car remainder))
             (cons first (cons nth (pair-deck (cdr remainder))))))))

; Scan a list and find the index of the ith entry with a non-zero car
; while avoiding one particular designated slot.
; i.e. find the nth slot with a space available to play the card.
;
; If you don't want to avoid a particular slot, set avoid to -1.
;
; If i is greater than the number of valid slots then this code fails
; badly.
(define (find-hole-helper slots n i avoid)
  (cond ((null? slots) -1) ; This should never happen with correct input
       ((or (= (car (car slots)) 0) ; Skip empty slots
            (= n avoid))            ; and the avoided slot
        (find-hole-helper (cdr slots) (+ 1 n) i avoid))
       ((= i 0) n) ; The loop terminates on a valid slot
       (#t (find-hole-helper (cdr slots) (+ 1 n) (- i 1) avoid))))

(define (find-hole slots n avoid)
  (find-hole-helper slots 0 n avoid))

; We know what the deepest element is, but we want to find its index.
(define (find-deepest holes deepest)
  (if (= (car holes) deepest)
      0
      (+ 1 (find-deepest (cdr holes) deepest))))

; Find the a slot suitable for the first card of a pair to be played in.
;
; Note the special logic needed so that we aren't stuck trying to play
; two cards into the same slot at the end.
(define (select-first slots)
  (let* ((holes (map (lambda (x) (car x)) slots))
        (deepest (apply max holes))
        (total (apply + holes))
        (n (apply + (map (lambda (x) (if (= (car x) 0) 0 1)) slots)))
        (r (aisleriot-random n)))
    (if (<= total (* deepest 2)) ; Catch the case where we are forced to
                                ; fill in a hole
       (find-deepest holes deepest)
       (find-hole slots r -1))))

(define (select-second slots first)
  (let* ((n (apply + (map (lambda (x) (if (= (car x) 0) 0 1)) slots)))
        (r (aisleriot-random (- n 1))))
    (find-hole slots r first)))

; Accepts a source slot and a list of count, slot-id pairs and distributes
; the cards in a way that is guranteed to produce a solvable game.
(define (deal-cards-in-pairs source slots)
  (if (empty-slot? source)
      '()
      ;; Note that everything is done as side-effects since that is the
      ;; way that it is done for the system deal-cards procedure.
      ;;
      ;; FIXME: The switching between vectors and lists is a little awkward.
      (let* ((vslots (list->vector slots))
            (a (select-first slots))
            (b (select-second slots a))
            (a-pair (vector-ref vslots a))
            (b-pair (vector-ref vslots b))
            (a-slot (cadr a-pair))
            (b-slot (cadr b-pair)))
       ;; Deal the cards
       (add-card! a-slot (make-visible (remove-card source)))
       (add-card! b-slot (make-visible (remove-card source)))
       ;; Update the list of potential slots
       (vector-set! vslots a (list (- (car a-pair) 1) a-slot))
       (vector-set! vslots b (list (- (car b-pair) 1) b-slot))
       (deal-cards-in-pairs source (vector->list vslots)))))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)
  (set! DECK (pair-deck DECK))
  
  (add-extended-slot '() right)

  (add-carriage-return-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot DECK)

  (deal-cards-in-pairs 11 '((2 0) (5 1) (5 2) (5 3) (5 4) (5 5)
                            (5 6) (5 7) (5 8) (5 9) (5 10)))

  ;; Uncomment this and comment out the expression above if you want 
  ;; to face unwinnable games as well.
  ;;(deal-cards-face-up 11 '(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1
  ;; 			     2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1
  ;;			     2 3 4 5 6 7 8 9 10 0 0))

  (list 10 4))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (= (length card-list) 1)))

(define (droppable? start-slot card-list end-slot)
  (and (not (empty-slot? end-slot))
       (not (= start-slot end-slot))
       (or (= 11 (+ (get-value (get-top-card end-slot))
		    (get-value (car card-list))))
	   (and (= 11 (get-value (get-top-card end-slot)))
		(= 11 (get-value (car card-list))))
	   (and (> (get-value (get-top-card end-slot)) 11)
		(> (get-value (car card-list)) 11)
		(not (= (get-value (get-top-card end-slot))
			(get-value (car card-list))))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (remove-card end-slot)
       (add-to-score! 2)))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 2)
       (empty-slot? 3)
       (empty-slot? 4)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 9)
       (empty-slot? 10)))

(define (check-for-pairs slot1 slot2)
  (cond ((= slot1 10)
	 #f)
	((or (empty-slot? slot1)
	     (= slot2 11))
	 (check-for-pairs (+ 1 slot1) (+ 2 slot1)))
	((and (not (empty-slot? slot2))
	      (is-pair? (get-top-card slot1) (get-top-card slot2)))
	 (hint-move slot1 1 slot2))
	(#t (check-for-pairs slot1 (+ 1 slot2)))))

(define (get-hint)
  (check-for-pairs 0 1))

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
