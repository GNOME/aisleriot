; AisleRiot - camelot.scm
; Copyright (C) 1998, 2003 Rosanna Yuen <rwsy@mit.edu>
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

(def-save-var add-stage #t)
(def-save-var fill-count 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)
 
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-normal-slot '() 'corner)           ; Slot 0
  (add-normal-slot '() 'top)              ; Slot 1
  (add-normal-slot '() 'top)              ; Slot 2
  (add-normal-slot '() 'corner)           ; Slot 3
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-normal-slot '() 'left)     ; Slot 4
  (add-normal-slot '() 'tableau)  ; Slot 5
  (add-normal-slot '() 'tableau)  ; Slot 6
  (add-normal-slot '() 'right)    ; Slot 7
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-normal-slot '() 'left)     ; Slot 8
  (add-normal-slot '() 'tableau)  ; Slot 9
  (add-normal-slot '() 'tableau)  ; Slot 10
  (add-normal-slot '() 'right)    ; Slot 11
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-normal-slot '() 'corner)   ; Slot 12
  (add-normal-slot '() 'bottom)   ; Slot 13
  (add-normal-slot '() 'bottom)   ; Slot 14
  (add-normal-slot '() 'corner)   ; Slot 15

  (set! HORIZPOS 0)
  (set! VERTPOS 0)

  (add-normal-slot DECK 'stock)   ; Slot 16
  (add-normal-slot '() 'waste)    ; Slot 17
  (set! add-stage #t)
  (set! fill-count 0)

  (give-status-message)

  (list 7 4)
)

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards 16)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (if (= slot-id 17)
	   (and (< fill-count 16)
		(set! add-stage #t) 
		#t)
	   (not (or add-stage
		    (= slot-id 16)
		    (= (get-value (car card-list)) king)
		    (= (get-value (car card-list)) queen)
		    (= (get-value (car card-list)) jack))))))

(define (droppable? start-slot card-list end-slot)
  (if add-stage
      (and (not (= end-slot 17))
           (empty-slot? end-slot)
	   (cond ((= (get-value (car card-list)) king)
		  (member end-slot '(0 3 12 15)))
		 ((= (get-value (car card-list)) queen)
		  (member end-slot '(1 2 13 14)))
		 ((= (get-value (car card-list)) jack)
		  (member end-slot '(4 7 8 11)))
		 (#t (not (= end-slot 16)))))
      (if (= start-slot end-slot)
	  (= (get-value (car card-list)) 10)
	  (and (not (empty-slot? end-slot))
	       (not (> end-slot 15))
	       (= 10 (+ (get-value (car card-list))
			(get-value (car (get-cards end-slot)))))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (cond (add-stage
              (move-n-cards! start-slot end-slot card-list)
              (or (> end-slot 15)
                  (set! fill-count (+ fill-count 1))))
             ((= start-slot end-slot)
              (set! fill-count (- fill-count 1)))
             (#t
	      (remove-card end-slot)
	      (set! fill-count (- fill-count 2))))))

(define (dealable?)
  (and (empty-slot? 17)
       (or (empty-slot? 0)
           (empty-slot? 1)
           (empty-slot? 2)
           (empty-slot? 3)
           (empty-slot? 4)
           (empty-slot? 5)
           (empty-slot? 6)
           (empty-slot? 7)
           (empty-slot? 8)
           (empty-slot? 9)
           (empty-slot? 10)
           (empty-slot? 11)
           (empty-slot? 12)
           (empty-slot? 13)
           (empty-slot? 14)
           (empty-slot? 15))))

(define (do-deal-next-cards)
  (set! add-stage #t)
  (flip-stock 16 17 0))

(define (button-clicked slot-id)  
  (if (= slot-id 16)
      (and (dealable?)
           (do-deal-next-cards))
      (and (not add-stage)
	   (not (empty-slot? slot-id))
	   (is-visible? (get-top-card slot-id))
	   (= 10 (get-value (get-top-card slot-id)))
	   (set! fill-count (- fill-count 1))
	   (remove-card slot-id))))

(define (button-double-clicked slot)
  #f)     

(define (game-won)
  (and (empty-slot? 16)
       (empty-slot? 17)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 9)
       (empty-slot? 10)))

(define (hint-remove-ten suit)
  (cond ((eq? suit club) (G_"Remove the ten of clubs."))
        ((eq? suit diamond) (G_"Remove the ten of diamonds."))
        ((eq? suit heart) (G_"Remove the ten of hearts."))
        ((eq? suit spade) (G_"Remove the ten of spades."))))

(define (find-match slot1 slot2)
  (cond ((= slot2 16) (find-match (+ 1 slot1) 0))
        ((= slot1 16) #f)
        ((or (empty-slot? slot2) (> (get-value (get-top-card slot2)) 10)) (find-match slot1 (+ 1 slot2)))
        ((or (empty-slot? slot1) (> (get-value (get-top-card slot1)) 10)) (find-match (+ 1 slot1) 0))
        ((= 10 (get-value (get-top-card slot2))) (list 0 (hint-remove-ten (get-suit (get-top-card slot2)))))
        ((= slot1 slot2) (find-match slot1 (+ 1 slot2)))
        ((= 10 (+ (get-value (get-top-card slot1)) (get-value (get-top-card slot2))))
         (hint-move slot1 1 slot2))
        (#t (find-match slot1 (+ 1 slot2)))))

(define (placeable? card)
  (cond ((= (get-value card) king)
	 (find-empty-slot '(0 3 12 15)))
	((= (get-value card) queen)
	 (find-empty-slot '(1 2 13 14)))
	((= (get-value card) jack)
	 (find-empty-slot '(4 8 7 11)))
	(#t
	 (find-empty-slot '(5 6 9 10 0 1 2 3 4 7 8 11 12 13 14 15)))))

(define (game-over)
  (give-status-message)
  (if (or (= fill-count 16)
	  (and (empty-slot? 16) (empty-slot? 17)))
      (begin 
	(set! add-stage #f)
	(find-match 0 0))
      (or (empty-slot? 17)
	  (placeable? (get-top-card 17)))))

(define (get-hint)
  (or (if add-stage
	  (and (not (empty-slot? 17))
	       (hint-move 17 1 (placeable? (get-top-card 17))))
	  (find-match 0 0))
      (list 0 (G_"Deal a new card from the deck"))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature scores-disabled dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable? dealable?)


