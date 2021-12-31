; AisleRiot - pileon.scm
; Copyright (C) 1998 Nick Lamb <njl195@zepler.org.uk>
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

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 1))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (deal-cards-face-up-from-deck DECK
   '(0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12))

  (freeze-slots-if-complete '(0 1 2 3 4 5 6 7 8 9 10 11 12))

  (list 8 4))

(define (check-same-value-list card-list)
  (if (< (length card-list) 2)
      #t
      (if (= (get-value (car card-list)) (get-value (cadr card-list)))
          (check-same-value-list (cdr card-list))
          #f)))

(define (freeze-slot slot-id)
   (flip-top-card slot-id)
   (add-to-score! 4))

(define (button-pressed slot-id card-list)
   (and (check-same-value-list card-list)
        (is-visible? (car card-list))))

(define (freeze-if-complete slot-id)
  (and (= (length (get-cards slot-id)) 4)
       (check-same-value-list (get-cards slot-id))
       (freeze-slot slot-id))
  #t)

(define (freeze-slots-if-complete slots)
  (and (not (null? slots))
       (freeze-if-complete (car slots))
       (freeze-slots-if-complete (cdr slots))))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (freeze-if-complete end-slot))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (empty-slot? end-slot)
           (eq? (get-value (car (get-cards end-slot)))
                (get-value (car card-list))))
       (< (+ (length (get-cards end-slot)) (length card-list)) 5)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (complete-transaction start-slot card-list end-slot)))

(define (button-clicked slot-id) #f)

(define (button-double-clicked slot) #f)

(define (game-over) 
  (and (not (game-won))
       (get-hint)))

(define (done-or-empty slot-id)
  (or (empty-slot? slot-id)
      (not (is-visible? (car (get-cards slot-id))))))

(define (game-won) 
  (and (done-or-empty 0)
       (done-or-empty 1)
       (done-or-empty 2)
       (done-or-empty 3)
       (done-or-empty 4)
       (done-or-empty 5)
       (done-or-empty 6)
       (done-or-empty 7)
       (done-or-empty 8)
       (done-or-empty 9)
       (done-or-empty 10)
       (done-or-empty 11)
       (done-or-empty 12)
       (done-or-empty 13)
       (done-or-empty 14))
)

(define (check-number slot-id)
  (cond ((and (> (length (get-cards slot-id)) 1)
	      (not (= (get-value (get-top-card slot-id))
		      (get-value (cadr (get-cards slot-id))))))
	 1)
	((and (> (length (get-cards slot-id)) 2)
	      (not (= (get-value (get-top-card slot-id))
		      (get-value (caddr (get-cards slot-id))))))
	 2)
	((and (> (length (get-cards slot-id)) 3)
	      (not (= (get-value (get-top-card slot-id))
		      (get-value (cadddr (get-cards slot-id))))))
	 3)
	(#t 1)))

(define (check-a-slot slot-id number-to-move to-slot)
  (if (> to-slot 14)
      #f
      (if (= slot-id to-slot)
	  (check-a-slot slot-id number-to-move (+ 1 to-slot))
	  (cond ((empty-slot? slot-id)
		 (list 2 (G_"something") (G_"an empty slot")))
		((empty-slot? to-slot)
		 (list 2 (G_"something") (G_"an empty slot")))
		((= 4 (length (get-cards to-slot)))
		 (check-a-slot slot-id number-to-move (+ 1 to-slot)))
		((= (get-value (get-top-card to-slot))
		    (get-value (get-top-card slot-id)))
		 (if (> number-to-move (- 4 (length (get-cards to-slot))))
		     (check-a-slot slot-id 
				   (- number-to-move 
				      (- 4 (length (get-cards to-slot))))
				   (+ 1 to-slot))
		     (hint-move slot-id 1 to-slot)))
		(#t (check-a-slot slot-id number-to-move (+ 1 to-slot)))))))

(define (check-slots slot-id to-slot)
  (if (> slot-id 14)
      #f
      (or (check-a-slot slot-id (check-number slot-id) to-slot)
	  (check-slots (+ 1 slot-id) 0))))

(define (get-hint)
  (check-slots 0 1))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
