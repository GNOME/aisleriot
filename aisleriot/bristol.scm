; AisleRiot - bristol.scm
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
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))
  (add-extended-slot '() right)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))
  (add-extended-slot '() right)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))
  (add-extended-slot '() right)

  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))
  (add-extended-slot '() right)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))
  (add-extended-slot '() right)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact (truncate (+ HORIZPOS
					     (* 3 (/ (get-card-width) 4))))))
  (add-extended-slot '() right)

  (deal-cards-face-up 0 '(8 9 10 11 12 13 14 15 
			    8 9 10 11 12 13 14 15 
			    8 9 10 11 12 13 14 15))
  (check-for-kings 8)

  (give-status-message)

  (list 10 3))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (check-for-kings slot-id)
  (if (> slot-id 15)
      #f
      (begin
	(if (= (get-value (get-top-card slot-id)) king)
	    (begin
	      (let ((new-deck (get-cards slot-id)))
		(set-cards! slot-id (list (cadr new-deck)
					  (caddr new-deck)
					  (car new-deck))))
	      (if (= (get-value (get-top-card slot-id)) king)
		  (begin
		    (let ((new-deck (get-cards slot-id)))
		      (set-cards! slot-id (list (cadr new-deck)
						(caddr new-deck)
						(car new-deck)))))))
	    (if (= (get-value (cadr (get-cards slot-id))) king)
		(let ((new-deck (get-cards slot-id)))
		  (set-cards! slot-id (list (car new-deck)
					    (caddr new-deck)
					    (cadr new-deck))))))
	(check-for-kings (+ 1 slot-id)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (not (= slot-id 0))
       (not (and (> slot-id 3)
		 (< slot-id 8)))
       (= (length card-list) 1)))

(define (button-released start-slot card-list end-slot)
  (cond ((and (> end-slot 3)
	      (< end-slot 8))
	 (cond ((not (empty-slot? end-slot))
		(if (= (+ 1 (get-value (get-top-card end-slot)))
		       (get-value (car card-list)))
		    (begin
		      (add-to-score! 1)
		      (move-n-cards! start-slot end-slot card-list))
		    #f))
	       ((= (get-value (car card-list)) ace)
		(begin
		  (add-to-score! 1)
		  (move-n-cards! start-slot end-slot card-list)))
	       (#t #f)))
	((and (> end-slot 7)
	      (not (empty-slot? end-slot))
	      (= (get-value (get-top-card end-slot))
		 (+ 1 (get-value (car card-list)))))
	 (move-n-cards! start-slot end-slot card-list))
	(#t #f)))

(define (button-clicked slot-id)
  (if (and (= slot-id 0)
	   (not (empty-slot? 0)))
      (if (> (length (get-cards slot-id)) 3)
	  (deal-cards-face-up 0 '(1 2 3))
	  (deal-cards-face-up 0 '(1)))
      #f))

(define (move-to-foundations? slot-id f-slot)
  (cond ((= f-slot 8)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-value (get-top-card slot-id))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (begin
	   (add-to-score! 1)
	   (deal-cards slot-id (list f-slot))))
	(#t
	 (move-to-foundations? slot-id (+ 1 f-slot)))))

(define (button-double-clicked slot-id)
  (if (and (> slot-id 0)
	   (or (< slot-id 4)
	       (> slot-id 7))
	   (not (empty-slot? slot-id)))
      (if (= ace  (get-value (get-top-card slot-id)))
	  (begin
	    (add-to-score! 1)
	    (cond ((empty-slot? 4)
		   (deal-cards slot-id '(4)))
		  ((empty-slot? 5)
		   (deal-cards slot-id '(5)))
		  ((empty-slot? 6)
		   (deal-cards slot-id '(6)))
		  (#t
		   (deal-cards slot-id '(7)))))
	  (move-to-foundations? slot-id 4))
      #f))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= 13 (length (get-cards 4)))
       (= 13 (length (get-cards 5)))
       (= 13 (length (get-cards 6)))
       (= 13 (length (get-cards 7)))))

(define (check-a-foundation slot-id foundation-id)
  (if (or (> foundation-id 7)
	  (empty-slot? slot-id))
      #f
      (cond ((and (empty-slot? foundation-id)
		  (= (get-value (get-top-card slot-id)) ace))
	     (list 2
		   (get-name (get-top-card slot-id)) 
		   "an empty Foundation pile"))
	    ((and (not (empty-slot? foundation-id))
		  (= (+ 1 (get-value (get-top-card foundation-id)))
		     (get-value (get-top-card slot-id))))
	     (list 1
		   (get-name (get-top-card slot-id))
		   (get-name (get-top-card foundation-id))))
	    (#t (check-a-foundation slot-id (+ 1 foundation-id))))))

(define (check-to-foundations slot-id)
  (cond ((> slot-id 15)
	 #f)
	((= slot-id 4)
	 (check-to-foundations 8))
	((check-a-foundation slot-id 4)
	 (check-a-foundation slot-id 4))
	(#t
	 (check-to-foundations (+ 1 slot-id)))))

(define (check-reserve slot-id)
  (if (> slot-id 3)
      #f
      (or (and (not (empty-slot? slot-id))
	       (check-a-tslot slot-id 
			      (get-cards slot-id)
			      1
			      8))
	  (check-reserve (+ 1 slot-id)))))

(define (deepness card-list temp-deepness)
  (if (and (> (length card-list) 1)
	   (= (+ 1 (get-value (car card-list)))
	      (get-value (cadr card-list))))
      (deepness (cdr card-list) (+ 1 temp-deepness))
      temp-deepness))

(define (check-a-tslot slot1 card-list depth slot2)
  (if (or (> slot2 15)
	  (and (= (length card-list) depth)
	       (> slot1 7)))
      #f
      (if (and (not (empty-slot? slot2))
	       (not (= slot1 slot2))
	       (= (+ 1 (get-value (car card-list)))
		  (get-value (get-top-card slot2))))
	  (if (= depth 1)
	      (list 1 
		    (get-name (get-top-card slot1))
		    (get-name (get-top-card slot2)))

	      (and (check-a-tslot slot1 
				  (cdr card-list)
				  (- depth 1)
				  8)
		   (list 1 
			 (get-name (get-top-card slot1))
			 (get-name (get-top-card slot2)))))
	  (check-a-tslot slot1 card-list depth (+ 1 slot2)))))

(define (check-tableau slot-id)
  (if (> slot-id 15)
      #f
      (if (and (not (empty-slot? slot-id))
	       (check-a-tslot slot-id 
			      (get-cards slot-id) 
			      (deepness (get-cards slot-id) 1) 
			      8))
	  (check-a-tslot slot-id 
			      (get-cards slot-id) 
			      (deepness (get-cards slot-id) 1) 
			      8)
	  (check-tableau (+ 1 slot-id)))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 "Deal another round")))

(define (get-hint)
  (or (check-to-foundations 1)
      (check-reserve 1)
      (check-tableau 8)
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
