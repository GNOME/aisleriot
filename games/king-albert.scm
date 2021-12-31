; AisleRiot - king_albert.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
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

(define foundation '(0 1 2 3))
(define tableau '(4 5 6 7 8 9 10 11 12))
(define reserve '(13 14 15 16 17 18 19))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-blank-slot)
  (add-normal-slot DECK 'foundation)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (set! HORIZPOS 0)
  (set! VERTPOS 0)

  (add-carriage-return-slot)

  (set! VERTPOS (+ VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 9))
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 9))
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 9))
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 9))

  (set! HORIZPOS 0)
  (set! VERTPOS 0)

  (add-carriage-return-slot)

  (set! HORIZPOS (+ HORIZPOS 9))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 9))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 9))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)
  (add-carriage-return-slot)
  (set! HORIZPOS (+ HORIZPOS 9))
  (add-blank-slot)
  (add-normal-slot '() 'reserve)

  (deal-cards 0 '(4 5 6 7 8 9 10 11 12 4 5 6 7 8 9 10 11 4 5 6 7 8 9
		    10 4 5 6 7 8 9 4 5 6 7 8 4 5 6 7 4 5 6 4 5 4 13 14
		    15 16 17 18 19))

  (map flip-top-card '(4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

  (list 11 5))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (is-visible? (car (reverse card-list)))))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((< end-slot 4)
	 (and (= (length card-list) 1)
	      (or (and (= (get-value (car card-list)) ace)
		       (empty-slot? end-slot))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (get-top-card end-slot))
			  (get-suit (car card-list)))
		       (= (get-value (car card-list))
			  (+ 1 (get-value (get-top-card end-slot))))))))
	((< end-slot 13)
	 (and (or (empty-slot? end-slot)
		  (and (eq? (is-red? (car (reverse card-list)))
			    (is-black? (get-top-card end-slot)))
		       (= (+ 1 (get-value (car (reverse card-list))))
			  (get-value (get-top-card end-slot)))))))
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (or (> start-slot 3)
           (add-to-score! -1))
       (or (> end-slot 3)
           (add-to-score! 1))
       (or (empty-slot? start-slot)
           (make-visible-top-card start-slot))))

(define (button-clicked slot-id)
  #f)

(define (move-ace-to-foundation slot f-slot)
  (if (empty-slot? f-slot)
      f-slot
      (move-ace-to-foundation slot (+ 1 f-slot))))

(define (move-card-to-foundation slot f-slot)
  (cond ((= f-slot 4)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 f-slot)
	(#t (move-card-to-foundation slot (+ 1 f-slot)))))
	

(define (button-double-clicked slot-id)
  (and (> slot-id 3)
       (not (empty-slot? slot-id))
       (or (and (= (get-value (get-top-card slot-id))
		   ace)
		(deal-cards slot-id (list (move-ace-to-foundation slot-id 0))))
	   (and (move-card-to-foundation slot-id 0)
		(deal-cards slot-id (list (move-card-to-foundation slot-id 0)))))
       (add-to-score! 1)
       (or (empty-slot? slot-id)
	   (make-visible-top-card slot-id))))

(define (game-continuable)
  (not (game-won)))

(define (game-won)
  (and (= (length (get-cards 0)) 13)
       (= (length (get-cards 1)) 13)
       (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)))

(define (strip card-list)
  (if (or (= (length card-list) 1)
	  (not (is-visible? (cadr card-list))))
      (car card-list)
      (strip (cdr card-list))))

(define (strip-size card-list n)
  (if (or (= (length card-list) 1)
	  (not (is-visible? (cadr card-list))))
      (+ n 1)
      (strip-size (cdr card-list) (+ n 1))))

(define (check-plop card t-slot)
  (cond ((= t-slot 13)
	 #f)
	((and (not (empty-slot? t-slot))
	      (eq? (is-red? card)
		   (is-black? (get-top-card t-slot)))
	      (= (+ 1 (get-value card))
		 (get-value (get-top-card t-slot))))
	 t-slot)
	((empty-slot? t-slot)
	 t-slot)
	(#t (check-plop card (+ 1 t-slot)))))

(define (check-uncover t-slot)
  (cond ((= t-slot 20)
	 #f)
	((and (not (empty-slot? t-slot))
	      (check-plop (strip (get-cards t-slot)) 4))
	 (hint-move t-slot (strip-size (get-cards t-slot) 0) (check-plop (strip (get-cards t-slot)) 4)))
	((and (not (empty-slot? t-slot))
	      (> (length (get-cards t-slot)) 1)
	      (not (is-visible? (cadr (get-cards t-slot))))
	      (check-a-slot-to-foundations t-slot 0))
	 (check-a-slot-to-foundations t-slot 0))
	(#t (check-uncover (+ 1 t-slot)))))

(define (check-a-foundation-for-uncover card f-slot)
  (cond ((= f-slot 4)
	 #f)
	((and (not (empty-slot? f-slot))
	      (eq? (is-red? card)
		   (is-black? (get-top-card f-slot)))
	      (= (+ 1 (get-value card))
		 (get-value (get-top-card f-slot)))
	      (check-plop (get-top-card f-slot) 4))
	 (hint-move f-slot 1 (check-plop (get-top-card f-slot) 4)))
	(#t (check-a-foundation-for-uncover card (+ 1 f-slot)))))

(define (check-foundation-for-uncover t-slot)
  (cond ((= t-slot 13)
	 #f)
	((and (not (empty-slot? t-slot))
	      (not (is-visible? (car (reverse (get-cards t-slot)))))
	      (check-a-foundation-for-uncover (strip (get-cards t-slot)) 0))
	 (check-a-foundation-for-uncover (strip (get-cards t-slot)) 0))
	(#t (check-foundation-for-uncover (+ 1 t-slot)))))

(define (check-empty-tslot t-slot)
  (cond ((= t-slot 13)
	 #f)
	((and (not (empty-slot? t-slot))
	      (is-visible? (car (reverse (get-cards t-slot))))
	      (check-plop (car (reverse (get-cards t-slot))) 4))
	 (if (empty-slot? (check-plop (car (reverse (get-cards t-slot))) 4))
	     (check-empty-tslot (+ 1 t-slot))
	     (hint-move t-slot (length (get-cards t-slot)) (check-plop (car (reverse (get-cards t-slot))) 4))))
	(#t (check-empty-tslot (+ 1 t-slot)))))

(define (check-to-foundations slot f-slot)
  (cond ((= slot 20)
	 #f)
	((empty-slot? slot)
	 (check-to-foundations (+ 1 slot) f-slot))
	((check-a-slot-to-foundations slot f-slot)
	 (check-a-slot-to-foundations slot f-slot))
	(#t (check-to-foundations (+ 1 slot) f-slot))))

(define (check-a-slot-to-foundations slot f-slot)
  (cond ((= f-slot 4)
	 #f)
	((= (get-value (get-top-card slot))
	    ace)
	 (hint-move slot 1 (find-empty-slot foundation)))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (hint-move slot 1 f-slot))
	(#t (check-a-slot-to-foundations slot (+ 1 f-slot)))))
	
(define (check-simple-foundation slot happynum)
  (cond ((= slot 20)
	 #f)
	((and (not (empty-slot? slot))
	      (<= (get-value (get-top-card slot)) happynum)
	      (check-a-slot-to-foundations slot 0))
	 (check-a-slot-to-foundations slot 0))
	(#t (check-simple-foundation (+ 1 slot) happynum))))

(define (get-min-happy-foundation fslot value)
  (cond ((= fslot 4)
	 value)
	((empty-slot? fslot)
	 2)
	(#t (get-min-happy-foundation (+ 1 fslot) 
				      (min value (+ 2 (get-value (get-top-card fslot))))))))

(define (get-hint)
  (or (check-simple-foundation 4 (get-min-happy-foundation 2 king))
      (check-uncover 4)
      (check-empty-tslot 4)
      (check-foundation-for-uncover 4)
      (check-to-foundations 4 0)
      (list 0 (G_"Try rearranging the cards"))))

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
