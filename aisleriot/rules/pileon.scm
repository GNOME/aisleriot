; AisleRiot - pileon.scm
; Copyright (C) 1998 Nick Lamb <njl195@zepler.org.uk>
;
; This game is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
; USA

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (set! HORIZPOS (+ HORIZPOS 70))
  (add-partially-extended-slot '() right 4)
  (add-carriage-return-slot)

  (deal-cards-face-up-from-deck DECK
   '(0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7 7 7 7 8 8 8 8 9 9 9 9 10 10 10 10 11 11 11 11 12 12 12 12))

  (list 7 4))

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

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (and (= (length (get-cards end-slot)) 4)
           (check-same-value-list (get-cards end-slot)))
                (freeze-slot end-slot)) #t)

(define (button-released start-slot card-list end-slot)
  (and (or (empty-slot? end-slot)
           (eq? (get-value (car (get-cards end-slot)))
                (get-value (car card-list))))
       (< (+ (length (get-cards end-slot)) (length card-list)) 5)
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
		 (list 2 "something" "an empty slot"))
		((empty-slot? to-slot)
		 (list 2 "something" "an empty slot"))
		((= 4 (length (get-cards to-slot)))
		 (check-a-slot slot-id number-to-move (+ 1 to-slot)))
		((= (get-value (get-top-card to-slot))
		    (get-value (get-top-card slot-id)))
		 (if (> number-to-move (- 4 (length (get-cards to-slot))))
		     (check-a-slot slot-id 
				   (- number-to-move 
				      (- 4 (length (get-cards to-slot))))
				   (+ 1 to-slot))
		     (list 2 (get-name (get-top-card slot-id))
			   (get-name (get-top-card to-slot)))))
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

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
