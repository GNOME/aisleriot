; AisleRiot - lady_jane.scm
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

(define BASE-VAL 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())
  
  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)
 
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (get-vert-start))

  (set! VERTPOS (inexact->exact (truncate (+ VERTPOS
					     (/ (get-card-height) 2)))))
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))
  (add-normal-slot '())
  (add-carriage-return-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))
  (add-normal-slot '())
  (add-carriage-return-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))
  (add-normal-slot '())
  (add-carriage-return-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))


  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (get-vert-start))

  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))
  (add-blank-slot)
  (add-normal-slot '())
  (add-carriage-return-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS 
					      (* (/ 15 2) 
						 (get-card-width))))))
  (add-blank-slot)
  (add-normal-slot '())

  (deal-cards 0 '(7 8 9 10 11 12 8 9 10 11 12 9 10 11 12 10 11 12
		    11 12 12))
  (deal-cards-face-up 0 '(6 7 8 9 10 11 12 13 14 15 16 17 18 19 2))

  (add-to-score! 1)

  (set! BASE-VAL (get-value (get-top-card 2)))

  (give-status-message)

  (list 9 4)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-base-string))))

(define (get-base-string)
  (cond ((and (> BASE-VAL 1)
	      (< BASE-VAL 11))
	 (string-append "Base Card:  " (number->string BASE-VAL)))
	((= BASE-VAL 1)
	 "Base Card:  Ace")
	((= BASE-VAL 11)
	 "Base Card:  Jack")
	((= BASE-VAL 12)
	 "Base Card:  Queen")
	((= BASE-VAL 13)
	 "Base Card:  King")
	(#t #f)))

(define (get-stock-no-string)
  (if (> (length (get-cards 0)) 1)
      (string-append "Stock left:  " 
		     (number->string (length (get-cards 0))))
      (string-append "Stock left:  0"))) 

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (is-visible? (car (reverse card-list)))))

(define (to-foundation? card end-slot)
  (if (empty-slot? end-slot)
      (= (get-value card) BASE-VAL)
      (and (eq? (get-suit card)
		(get-suit (get-top-card end-slot)))
	   (or (= (+ 1 (get-value (get-top-card end-slot)))
		  (get-value card))
	       (and (= (get-value (get-top-card end-slot)) king)
		    (= (get-value card) ace))))))

(define (to-tableau? card end-slot)
  (if (empty-slot? end-slot)
      (or (= (get-value card) (- BASE-VAL 1))
	  (and (= BASE-VAL ace)
	       (= (get-value card) king)))
      (and (not (eq? (is-red? card)
		     (is-red? (get-top-card end-slot))))
	   (not (= (get-value (get-top-card end-slot)) BASE-VAL))
	   (or (= (get-value (get-top-card end-slot))
		  (+ 1 (get-value card)))
	       (and (= (get-value (get-top-card end-slot)) ace)
		    (= (get-value card) king))))))

(define (button-released start-slot card-list end-slot)
  (cond ((and (> end-slot 1)
	      (< end-slot 6))
	 (and (= (length card-list) 1)
	      (to-foundation? (car card-list) end-slot)
	      (or (and (> start-slot 5)
		       (< start-slot 13)
		       (not (empty-slot? start-slot))
		       (make-visible-top-card start-slot))
		  (and (> start-slot 1)
		       (< start-slot 6)
		       (add-to-score! -1))
		  #t)
	      (add-to-score! 1)
	      (move-n-cards! start-slot end-slot card-list)))
	((and (> end-slot 5)
	      (< end-slot 13))
	 (and (to-tableau? (car (reverse card-list)) end-slot)
	      (or (and (> start-slot 1)
		       (< start-slot 6)
		       (add-to-score! -1))
		  (and (> start-slot 5)
		       (< start-slot 13)
		       (not (empty-slot? start-slot))
		       (make-visible-top-card start-slot))
		  #t)
	      (move-n-cards! start-slot end-slot card-list)))
	(#t #f)))

(define (button-clicked slot-id)
  (if (= slot-id 0)
      (cond ((> (length (get-cards slot-id)) 7)
	     (and (deal-cards-face-up 0 '(13 14 15 16 17 18 19))
		  (give-status-message)))
	    ((> (length (get-cards slot-id)) 1)
	     (and (deal-cards-face-up 0 '(1))
		  (make-visible-top-card 0)
		  (give-status-message)))
	    (#t #f))
      #f))

(define (move-to-foundations? card slot-id)
  (cond ((> slot-id 5)
	 #f)
	((to-foundation? card slot-id)
	 (add-card! slot-id card))
	(#t
	 (move-to-foundations? card (+ 1 slot-id)))))

(define (button-double-clicked slot-id)
  (if (or (empty-slot? slot-id)
	  (and (> slot-id 2)
	       (< slot-id 6))
	  (not (is-visible? (get-top-card slot-id))))
      #f
      (and (move-to-foundations? (get-top-card slot-id) 2)
	   (remove-card slot-id)
	   (add-to-score! 1)
	   (or (empty-slot? slot-id)
	       (> slot-id 12)
	       (< slot-id 2)
	       (make-visible-top-card slot-id)))))

(define (game-continuable)
  (not (game-won)))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)))

(define (dealable?)
  (and (> (length (get-cards 0)) 1)
       (list 0 "Deal another round")))

(define (check-a-foundation slot1 slot2)
  (if (< slot2 6)
      (or (to-foundation? (get-top-card slot1) slot2)
	  (check-a-foundation slot1 (+ 1 slot2)))
      #f))

(define (check-to-foundations slot-id)
  (cond ((> slot-id 19)
	 #f)
	((= slot-id 2)
	 (check-to-foundations 6))
	((or (empty-slot? slot-id)
	     (not (is-visible? (get-top-card slot-id))))
	 (check-to-foundations (+ 1 slot-id)))
	((check-a-foundation slot-id 2)
	 (or (and (= (get-value (get-top-card slot-id)) BASE-VAL)
		  (list 2 
			(get-name (get-top-card slot-id)) 
			"an empty Foundation pile"))
	     (list 1 
		   (get-name (get-top-card slot-id))
		   (get-name 
		    (make-card (if (= ace 
				      (get-value (get-top-card slot-id)))
				   king
				   (- (get-value (get-top-card slot-id))
				      1))
			       (get-suit (get-top-card slot-id)))))))
	(#t
	 (check-to-foundations (+ 1 slot-id)))))

(define (check-to-tableaus slot-id)
  (cond ((>slot-id 19)
	 #f)
	((= slot-id 2)
	 (check-to-tableaus 6))
	((or (empty-slot? slot-id) 
	     (not (is-visible? (get-top-card slot-id))))
	 (check-to-tableaus (+ 1 slot-id)))
	(#t
	 (check-to-tableaus (+ 1 slot-id)))))

(define (get-hint)
  (or (check-to-foundations 0)
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
