; AisleRiot - gypsy.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
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
  (make-standard-double-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-blank-slot)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards 0 '(9 10 11 12 13 14 15 16 9 10 11 12 13 14 15 16))
  (deal-cards-face-up 0 '(9 10 11 12 13 14 15 16))

  (give-status-message)


  (list 10 5))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)
       (not (eq? '() card-list))
       (is-visible? (car (reverse card-list)))
       (check-alternating-color-list card-list)
       (check-straight-descending-list card-list)))

(define (remove-alternate-points slot card-list)
  (or (empty-slot? slot)
      (not (is-visible? (get-top-card slot)))
      (eq? (is-red? (get-top-card slot))
	   (is-red? (car (reverse card-list))))
      (not (= (get-value (get-top-card slot))
	      (+ 1 (get-value (car (reverse card-list))))))
      (add-to-score! -2)))

(define (check-visibility slot)
  (or (empty-slot? slot)
      (is-visible? (get-top-card slot))
      (make-visible-top-card slot)))

(define (remove-from-foundation-points slot)
  (or (> slot 8)
      (and (add-to-score! -5)
	   (or (< (length (get-cards slot)) 12)
	       (add-to-score! -60)))))

(define (button-released start-slot card-list end-slot)
  (cond ((= end-slot start-slot)
	 #f)
	((and (> end-slot 0)
	      (< end-slot 9))
	 (if (= (length card-list) 1)
	     (cond ((empty-slot? end-slot)
		    (and (= (get-value (car card-list)) ace)
			 (move-n-cards! start-slot end-slot card-list)
			 (or (and (> start-slot 0)
				  (< start-slot 9))
			     (add-to-score! 5))
			 (remove-alternate-points start-slot card-list)
			 (check-visibility start-slot)))
		   ((and (= (get-suit (get-top-card end-slot))
			    (get-suit (car card-list)))
			 (= (get-value (car card-list))
			    (+ 1 (get-value (get-top-card end-slot)))))
		    (and (move-n-cards! start-slot end-slot card-list)
			 (or (and (> start-slot 0)
				  (< start-slot 9))
			     (and (add-to-score! 5)
				  (or (< (length (get-cards end-slot)) 13)
				      (add-to-score! 60))))
			 (remove-alternate-points start-slot card-list)
			 (check-visibility start-slot)))
		   (#t #f))	     
	     #f))
	((and (> end-slot 8)
	      (empty-slot? end-slot))
	 (and (move-n-cards! start-slot end-slot card-list)
	      (remove-alternate-points start-slot card-list)
	      (check-visibility start-slot)))
	(#t (and (> end-slot 8)
		 (eq? (is-red? (get-top-card end-slot))
		      (is-black? (car (reverse card-list))))
		 (= (get-value (get-top-card end-slot))
		    (+ 1 (get-value (car (reverse card-list)))))
		 (move-n-cards! start-slot end-slot card-list)
		 (add-to-score! 2)
		 (remove-alternate-points start-slot card-list)
		 (remove-from-foundation-points start-slot)
		 (check-visibility start-slot)))))

(define (get-dealt-points slot)
  (cond ((> slot 16)
	 #t)
	((and (> (length (get-cards slot)) 1)
	      (is-visible? (cadr (get-cards slot)))
	      (eq? (is-red? (get-top-card slot))
		   (is-black? (cadr (get-cards slot))))
	      (= (+ 1 (get-value (get-top-card slot)))
		 (get-value (cadr (get-cards slot)))))
	 (and (add-to-score! 2)
	      (get-dealt-points (+ 1 slot))))
	(#t (get-dealt-points (+ 1 slot)))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? slot-id))
       (deal-cards-face-up 0 '(9 10 11 12 13 14 15 16))
       (get-dealt-points 9)))

(define (remove-alternate-points-on-dclick slot)
  (or (= (length (get-cards slot)) 1)
      (not (is-visible? (cadr (get-cards slot))))
      (eq? (is-red? (get-top-card slot))
	   (is-red? (cadr (get-cards slot))))
      (not (= (get-value (cadr (get-cards slot)))
	      (+ 1 (get-value (get-top-card slot)))))
      (add-to-score! -2)))

(define (find-empty-foundation a-slot f-slot)
  (cond ((> f-slot 8)
	 #f)
	((empty-slot? f-slot)
	 (deal-cards a-slot (list f-slot)))
	(#t (find-empty-foundation a-slot (+ 1 f-slot)))))

(define (find-foundation a-slot f-slot)
  (cond ((> f-slot 8)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card a-slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card a-slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (and (deal-cards a-slot (list f-slot))
	      (add-to-score! 5)))
	(#t (find-foundation a-slot (+ 1 f-slot)))))

(define tempscore 0)

(define (button-double-clicked slot-id)
  (set! tempscore (get-score))
  (and (> slot-id 8)
       (not (empty-slot? slot-id))
       (or (and (= (get-value (get-top-card slot-id))
		   ace)
		(remove-alternate-points-on-dclick slot-id)
		(find-empty-foundation slot-id 1)
		(check-visibility slot-id)
		(add-to-score! 5))
	   (and (remove-alternate-points-on-dclick slot-id)
		(or (find-foundation slot-id 1)
		    (not (set-score! tempscore)))
		(check-visibility slot-id)))))


(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 1)) 13)
       (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)
       (= (length (get-cards 6)) 13)
       (= (length (get-cards 7)) 13)
       (= (length (get-cards 8)) 13)))

(define (check-for-empty)
  (if (or (empty-slot? 9)
	   (empty-slot? 10)
	   (empty-slot? 11)
	   (empty-slot? 12)
	   (empty-slot? 13)
	   (empty-slot? 14)
	   (empty-slot? 15)
	   (empty-slot? 16))
      (list 0 "Move a card or build of cards on to the empty slot")
      #f))
       

(define (check-a-foundation card slot-id)
  (cond ((= slot-id 9)
	 #f)
	((and (not (empty-slot? slot-id))
	      (eq? (get-suit card)
		   (get-suit (get-top-card slot-id)))
	      (= (get-value card)
		 (+ 1 (get-value (get-top-card slot-id)))))
	 #t)
	(#t (check-a-foundation card (+ 1 slot-id)))))

(define (check-to-foundations? slot-id)
  (cond ((= slot-id 17)
	 #f)
	((empty-slot? slot-id)
	 (check-to-foundations? (+ 1 slot-id)))
	((= (get-value (get-top-card slot-id)) ace)
	 (list 2 (get-name (get-top-card slot-id)) "an empty Foundation"))
	((check-a-foundation (get-top-card slot-id) 1)
	 (list 1 
	       (get-name (get-top-card slot-id))
	       (get-name (make-card (- (get-value (get-top-card slot-id)) 1)
				    (get-suit (get-top-card slot-id))))))
	(#t (check-to-foundations? (+ 1 slot-id)))))

(define (stripped card-list card)
  (if (<= (length card-list) 1)
      '()
      (if (eq? card (car card-list))
	  (cdr card-list)
	  (if (= (length card-list) 2)
	      '()
	      (stripped (cdr card-list) card)))))

(define (check-a-tableau card slot1 card-list slot2 imbedded?)
  (cond ((or (= (length card-list) 0)
	     (not (is-visible? (car card-list))))
	 #f)
	((and (not (eq? (is-red? (car card-list))
			(is-red? card)))
	      (= (+ 1 (get-value (car card-list)))
		 (get-value card)))
	 (if (or (= (length card-list) 1)
		  (eq? (is-red? (car card-list))
		       (is-red? (cadr card-list)))
		  imbedded?
		  (not (= (+ 1 (get-value (car card-list)))
			  (get-value (cadr card-list))))
		  (check-a-foundation (cadr card-list) 0)
		  (and (check-alternating-color-list (list (car card-list) (cadr card-list)))
		       (check-straight-descending-list (list (car card-list) (cadr card-list)))
		       (check-a-tableau (get-top-card slot2)
					slot1	
					(cdr card-list)
					slot2
					#t))
		  (and (> (length (get-cards slot1)) 1)
		       (check-alternating-color-list (list (get-top-card slot1) 
							   (cadr (get-cards slot1))))
		       (check-straight-descending-list (list (get-top-card slot1) 
							     (cadr (get-cards slot1))))
		       (check-a-tableau (cadr card-list)
					slot2
					(get-cards slot1)
					slot1
					#t)))
	     (list 1 (get-name (car card-list)) (get-name card))
	     (and (not imbedded?)
		  (> (length card-list) 1)
		  (check-alternating-color-list (list (car card-list)
						      (cadr card-list)))
		  (check-straight-descending-list (list (car card-list)
						      (cadr card-list)))
		  (check-a-tableau card 
				   slot1 
				   (cdr card-list) 
				   slot2 
				   imbedded?))))
	(imbedded? #f)
	(#t (and (> (length card-list) 1)
		 (check-alternating-color-list (list (car card-list)
						     (cadr card-list)))
		 (check-straight-descending-list (list (car card-list)
						       (cadr card-list)))
		 (check-a-tableau card slot1 (cdr card-list) slot2 imbedded?)))))

(define (check-to-tableau? slot1 slot2)
  (cond ((= slot1 17)
	 #f)
	((or (= slot2 17)
	     (empty-slot? slot1))
	 (check-to-tableau? (+ 1 slot1) 9))
	((and (not (= slot1 slot2))
	      (check-a-tableau (get-top-card slot1) 
			       slot1 
			       (get-cards slot2) 
			       slot2 
			       #f))
	 (check-a-tableau (get-top-card slot1) 
			  slot1 
			  (get-cards slot2) 
			  slot2 
			  #f))
	(#t (check-to-tableau? slot1 (+ 1 slot2)))))

(define (check-for-deal)
  (if (not (empty-slot? 0))
      (list 0 "Deal another hand")
      #f))

(define (get-hint)
  (or (check-to-foundations? 9)
      (check-to-tableau? 9 10)
      (check-for-empty)
      (check-for-deal)))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
