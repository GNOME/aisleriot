; AisleRiot - zebra.scm
; Copyright (C) 1999 Rosanna Yuen <rwsy@mit.edu>
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
  (set! DECK (append (make-deck-list-ace-low 2 2 club) 
		     (make-deck-list-ace-low 2 2 club)))
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())

  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 3)))))

  (add-normal-slot (list (make-visible (make-card ace club))))
  (add-normal-slot (list (make-visible (make-card ace diamond))))
  (add-normal-slot (list (make-visible (make-card ace heart))))
  (add-normal-slot (list (make-visible (make-card ace spade))))
  (add-normal-slot (list (make-visible (make-card ace club))))
  (add-normal-slot (list (make-visible (make-card ace diamond))))
  (add-normal-slot (list (make-visible (make-card ace heart))))
  (add-normal-slot (list (make-visible (make-card ace spade))))

  (add-carriage-return-slot)

  (add-extended-slot '() down)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) (/ 58 21))))))
  (add-extended-slot '() down)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) (/ 58 21))))))
  (add-extended-slot '() down)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) (/ 58 21))))))
  (add-extended-slot '() down)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) (/ 58 21))))))
  (add-extended-slot '() down)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) (/ 58 21))))))
  (add-extended-slot '() down)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) (/ 58 21))))))
  (add-extended-slot '() down)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) (/ 58 21))))))
  (add-extended-slot '() down)

  (deal-cards-face-up 0 '(10 11 12 13 14 15 16 17))

  (give-status-message)

  (list 10 3)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (get-redeals-string)
  (string-append "Redeals left:  "
		 (number->string (- 1 FLIP-COUNTER))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 1)
	   (> slot-id 9))))

(define (empty-tableau slot)
  (or (not (empty-slot? slot))
      (= slot 1)
      (and (not (empty-slot? 1))
	   (deal-cards-face-up 1 (list slot)))
      (and (not (empty-slot? 0))
	   (deal-cards-face-up 0 (list slot))
	   (give-status-message))
      #t))

(define (button-released start-slot card-list end-slot)
  (cond ((> end-slot 9)
	 (and (= (length card-list) 1)
	      (not (empty-slot? end-slot))
	      (not (eq? (is-red? (car card-list))
			(is-red? (get-top-card end-slot))))
	      (= (get-value (get-top-card end-slot))
		 (+ 1 (get-value (car card-list))))
	      (move-n-cards! start-slot end-slot card-list)
	      (empty-tableau start-slot)))
	((> end-slot 1)
	 (and (not (eq? (is-red? (get-top-card end-slot))
			(is-red? (car card-list))))
	      (= (+ 1 (get-value (get-top-card end-slot)))
		 (get-value (car card-list)))
	      (move-n-cards! start-slot end-slot (reverse card-list))
	      (add-to-score! (length card-list))
	      (empty-tableau start-slot)
	      (give-status-message)))
	(#t #f)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (flip-stock 0 1 1)
       (give-status-message)))

(define (move-to-foundation slot-id foundation-id)
  (cond ((= foundation-id 10)
	 #f)
	((and (not (eq? (is-red? (get-top-card slot-id))
			(is-red? (get-top-card foundation-id))))
	      (= (+ 1 (get-value (get-top-card foundation-id)))
		 (get-value (get-top-card slot-id))))
	 (and (move-n-cards! slot-id
			     foundation-id
			     (list (get-top-card slot-id)))
	      (remove-card slot-id)
	      (empty-tableau slot-id)))
	(#t (move-to-foundation slot-id (+ 1 foundation-id)))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 1)
	   (> slot-id 9))
       (move-to-foundation slot-id 2)
       (add-to-score! 1)))

(define (game-continuable)
  (get-hint))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)
       (= (length (get-cards 6)) 13)
       (= (length (get-cards 7)) 13)
       (= (length (get-cards 8)) 13)
       (= (length (get-cards 9)) 13)))

(define (dealable?)
  (or (and (not (empty-slot? 0))
	   (list 0 "Deal another round"))
      (and (not (empty-slot? 1))
	   (< FLIP-COUNTER 1)
	   (list 0 "Move waste back to stock"))))

(define (check-a-foundation slot-id foundation-id)
  (cond ((= foundation-id 10)
	 #f)
	((and (not (eq? (is-red? (get-top-card slot-id))
			(is-red? (get-top-card foundation-id))))
	      (= (+ 1 (get-value (get-top-card foundation-id)))
		 (get-value (get-top-card slot-id))))
	 #t)
	(#t
	 (check-a-foundation slot-id (+ 1 foundation-id)))))

(define (check-to-foundations slot-id)
  (cond ((= slot-id 18)
	 #f)
	((= slot-id 2)
	 (check-to-foundations 10))
	((and (not (empty-slot? slot-id))
	      (check-a-foundation slot-id 2))
	 (list 2 
	       (get-name (get-top-card slot-id)) 
	       "the appropriate Foundation pile"))
	(#t
	 (check-to-foundations (+ 1 slot-id)))))

(define (check-a-tableau slot1 card-list slot2)
  (cond ((= slot2 18)
	 #f)
	((and (not (= slot1 slot2))
	      (not (eq? (is-red? (car card-list))
			(is-red? (get-top-card slot2))))
	      (= (+ 1 (get-value (car card-list)))
		 (get-value (get-top-card slot2)))
	      (or (= slot1 1)
		  (= (length card-list) 1)
		  (check-a-tableau slot1 (cdr card-list) 10)))
	 (list 1
	       (get-name (car card-list))
	       (get-name (get-top-card slot2))))
	(#t 
	 (check-a-tableau slot1 card-list (+ 1 slot2)))))

(define (check-to-tableaus slot-id)
  (cond ((= slot-id 18)
	 #f)
	((= slot-id 2)
	 (check-to-tableaus 10))
	((and (not (empty-slot? slot-id))
	      (check-a-tableau slot-id (get-cards slot-id) 10))
	 (check-a-tableau slot-id (get-cards slot-id) 10))
	(#t (check-to-tableaus (+ 1 slot-id)))))

(define (get-hint)
  (or (check-to-foundations 1)
      (check-to-tableaus 1)
      (dealable?)))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
