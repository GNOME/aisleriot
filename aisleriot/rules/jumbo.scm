; AisleRiot - jumbo.scm
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
  (add-normal-slot '())

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
  (add-extended-slot '() down)

  (deal-cards 0 '(10 11 12 13 14 15 16 17 18 10 11 12 13 14 15 16 17
		     10 11 12 13 14 15 16 10 11 12 13 14 15 10 11 12
		     13 14 10 11 12 13 10 11 12 10 11 10))

  (map flip-top-card '(10 11 12 13 14 15 16 17 18))

  (give-status-message)
  (list 11 4))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (get-redeals-string)
  (string-append "Redeals left:  "
		 (number->string (- 1 FLIP-COUNTER))))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (or (> slot-id 1)
	   (and (= slot-id 1)
		(= (length card-list) 1)))
       (is-visible? (car (reverse card-list)))))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (and (> start-slot 1)
	   (< start-slot 10))
      (add-to-score! -1))
  (if (and (> end-slot 1)
	   (< end-slot 10))
      (add-to-score! 1))
  (if (and (not (empty-slot? start-slot))
	   (> start-slot 9))
      (make-visible-top-card start-slot))
  #t)

(define (button-released start-slot card-list end-slot)
  (cond ((and (> end-slot 1)
	      (< end-slot 10))
	 (and (= (length card-list) 1)
	      (not (= start-slot end-slot))
	      (or (and (= (get-value (car card-list)) ace)
		       (empty-slot? end-slot))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (car card-list))
			  (get-suit (get-top-card end-slot)))
		       (= (get-value (car card-list))
			  (+ 1 (get-value (get-top-card end-slot))))))
	      (complete-transaction start-slot card-list end-slot)))
	((> end-slot 9)
	 (and (not (= start-slot end-slot))
	      (or (and (empty-slot? end-slot)
		       (= (get-value (car (reverse card-list))) king))
		  (and (not (empty-slot? end-slot))
		       (not (eq? (is-red? (get-top-card end-slot))
				 (is-red? (car (reverse card-list)))))
		       (= (get-value (get-top-card end-slot))
			  (+ (get-value (car (reverse card-list))) 1))))
	      (complete-transaction start-slot card-list end-slot)))
	(#t #f)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (flip-stock 0 1 1)))

(define (check-to-foundation card f-slot)
  (cond ((= f-slot 10)
	 #f)
	((or (and (empty-slot? f-slot)
		  (= (get-value card) ace))
	     (and (not (empty-slot? f-slot))
		  (= (get-suit card)
		     (get-suit (get-top-card f-slot)))
		  (= (get-value card)
		     (+ 1 (get-value (get-top-card f-slot))))))
	 f-slot)
	(#t (check-to-foundation card (+ 1 f-slot)))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 1)
	   (> slot-id 9))
       (check-to-foundation (get-top-card slot-id) 2)
       (deal-cards slot-id (list (check-to-foundation (get-top-card slot-id) 2)))
       (or (empty-slot? slot-id)
	   (make-visible-top-card slot-id))
       (add-to-score! 1)))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 10)
       (empty-slot? 11)
       (empty-slot? 12)
       (empty-slot? 13)
       (empty-slot? 14)
       (empty-slot? 15)
       (empty-slot? 16)
       (empty-slot? 17)
       (empty-slot? 18)))

(define (strip card-list)
  (if (not (is-visible? (cadr card-list)))
      (car card-list)
      (strip (cdr card-list))))

(define (check-plop card t-slot)
  (cond ((= t-slot 19)
	 #f)
	((and (not (empty-slot? t-slot))
	      (eq? (is-red? card)
		   (is-black? (get-top-card t-slot)))
	      (= (+ 1 (get-value card))
		 (get-value (get-top-card t-slot))))
	 t-slot)
	((and (empty-slot? t-slot)
	      (= (get-value card)
		 king))
	 t-slot)
	(#t (check-plop card (+ 1 t-slot)))))

(define (check-uncover t-slot)
  (cond ((= t-slot 19)
	 #f)
	((and (not (empty-slot? t-slot))
	      (not (is-visible? (car (reverse (get-cards t-slot)))))
	      (check-plop (strip (get-cards t-slot)) 10))
	 (if (empty-slot? (check-plop (strip (get-cards t-slot)) 10))
	     (list 2 
		   (get-name (strip (get-cards t-slot)))
		   "an empty tableau slot")
	     (list 1 
		   (get-name (strip (get-cards t-slot)))
		   (get-name (get-top-card (check-plop (strip (get-cards t-slot)) 
						       10))))))
	((and (not (empty-slot? t-slot))
	      (> (length (get-cards t-slot)) 1)
	      (not (is-visible? (cadr (get-cards t-slot))))
	      (check-a-slot-to-foundations t-slot 2))
	 (check-a-slot-to-foundations t-slot 2))
	(#t (check-uncover (+ 1 t-slot)))))

(define (check-a-foundation-for-uncover card f-slot)
  (cond ((= f-slot 10)
	 #f)
	((and (not (empty-slot? f-slot))
	      (eq? (is-red? card)
		   (is-black? (get-top-card f-slot)))
	      (= (+ 1 (get-value card))
		 (get-value (get-top-card f-slot)))
	      (check-plop (get-top-card f-slot) 10))
	 (list 1 
	       (get-name (get-top-card f-slot))
	       (get-name (get-top-card (check-plop (get-top-card f-slot) 10)))))
	(#t (check-a-foundation-for-uncover card (+ 1 f-slot)))))

(define (check-foundation-for-uncover t-slot)
  (cond ((= t-slot 19)
	 #f)
	((and (not (empty-slot? t-slot))
	      (not (is-visible? (car (reverse (get-cards t-slot)))))
	      (check-a-foundation-for-uncover (strip (get-cards t-slot)) 2))
	 (check-a-foundation-for-uncover (strip (get-cards t-slot)) 2))
	(#t (check-foundation-for-uncover (+ 1 t-slot)))))

(define (check-empty-tslot t-slot)
  (cond ((= t-slot 19)
	 #f)
	((and (not (empty-slot? t-slot))
	      (is-visible? (car (reverse (get-cards t-slot))))
	      (check-plop (car (reverse (get-cards t-slot))) 10))
	 (if (empty-slot? (check-plop (car (reverse (get-cards t-slot))) 10))
	     (check-empty-tslot (+ 1 t-slot))
	     (list 1
		   (get-name (car (reverse (get-cards t-slot))))
		   (get-name (get-top-card (check-plop (car (reverse (get-cards t-slot)))
						       10))))))
	(#t (check-empty-tslot (+ 1 t-slot)))))

(define (check-move-waste t-slot)
  (cond ((or (= t-slot 19)
	     (empty-slot? 1))
	 #f)
	((and (not (empty-slot? t-slot))
	      (eq? (is-red? (get-top-card 1))
		   (is-black? (get-top-card t-slot)))
	      (= (+ 1 (get-value (get-top-card 1)))
		 (get-value (get-top-card t-slot))))
	 (list 1 
	       (get-name (get-top-card 1))
	       (get-name (get-top-card t-slot))))
	((and (empty-slot? t-slot)
	      (= (get-value (get-top-card 1)) king))
	 (list 2
	       (get-name (get-top-card 1))
	       "an empty tableau slot"))
	((check-a-slot-to-foundations 1 2)
	 (check-a-slot-to-foundations 1 2))
	(#t (check-move-waste (+ 1 t-slot)))))

(define (check-to-foundations slot f-slot)
  (cond ((= slot 19)
	 #f)
	((empty-slot? slot)
	 (check-to-foundations (+ 1 slot) f-slot))
	((= slot 2)
	 (check-to-foundations 10 2))
	((check-a-slot-to-foundations slot f-slot)
	 (check-a-slot-to-foundations slot f-slot))
	(#t (check-to-foundations (+ 1 slot) f-slot))))

(define (check-a-slot-to-foundations slot f-slot)
  (cond ((= f-slot 10)
	 #f)
	((= (get-value (get-top-card slot))
	    ace)
	 (list 2
	       (get-name (get-top-card slot))
	       "an empty foundation"))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (list 1
	       (get-name (get-top-card slot))
	       (get-name (get-top-card f-slot))))
	(#t (check-a-slot-to-foundations slot (+ 1 f-slot)))))
	
(define (check-simple-foundation slot happynum)
  (cond ((= slot 19)
	 #f)
	((= slot 2)
	 (check-simple-foundation 10 happynum))
	((and (not (empty-slot? slot))
	      (<= (get-value (get-top-card slot)) happynum)
	      (check-a-slot-to-foundations slot 2))
	 (check-a-slot-to-foundations slot 2))
	(#t (check-simple-foundation (+ 1 slot) happynum))))

(define (dealable?)
  (or (and (not (empty-slot? 0))
	   (list 0 "Deal another card"))
      (and (not (empty-slot? 1))
	   (< FLIP-COUNTER 1)
	   (list 0 "Move waste to stock"))))

(define (get-min-happy-foundation fslot value)
  (cond ((= fslot 10)
	 value)
	((empty-slot? fslot)
	 2)
	(#t (get-min-happy-foundation (+ 1 fslot) 
				      (min value (+ 2 (get-value (get-top-card fslot))))))))

(define (check-foundation-for-waste f-slot)
  (cond ((or (= f-slot 10)
	     (empty-slot? 1))
	 #f)
	((and (not (empty-slot? f-slot))
	      (eq? (is-red? (get-top-card f-slot))
		   (is-black? (get-top-card 1)))
	      (= (get-value (get-top-card f-slot))
		 (+ 1 (get-value (get-top-card 1))))
	      (check-plop (get-top-card f-slot) 10))
	 (list 1
	       (get-name (get-top-card f-slot))
	       (get-name (get-top-card (check-plop (get-top-card f-slot) 10)))))
	(#t (check-foundation-for-waste (+ 1 f-slot)))))

(define (get-hint)
  (or (check-simple-foundation 1 (get-min-happy-foundation 2 king))
      (check-uncover 10)
      (check-empty-tslot 10)
      (check-move-waste 10)
      (check-foundation-for-uncover 10)
      (check-foundation-for-waste 2)
      (dealable?)
      (check-to-foundations 1 2)
      (list 0 "Try rearranging the cards")))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout)
