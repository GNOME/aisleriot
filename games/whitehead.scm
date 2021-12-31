; AisleRiot - whitehead.scm
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

(define foundation '(2 3 4 5))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)
  (add-normal-slot '() 'waste)

  (add-blank-slot)

  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)

  (add-carriage-return-slot)

  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (deal-cards-face-up 0 '(6 7 8 9 10 11 12 7 8 9 10 11 12 8 9 10 11 12
			    9 10 11 12 10 11 12 11 12 12))

  (give-status-message)

  (list 7 4))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)
       (check-same-suit-list card-list)
       (check-straight-descending-list card-list)))

(define (droppable? start-slot card-list end-slot)
  (cond ((> end-slot 5)
	 (and (not (= start-slot end-slot))
	      (or (empty-slot? end-slot)
		  (and (eq? (is-red? (get-top-card end-slot))
			    (is-red? (car card-list)))
		       (= (get-value (get-top-card end-slot))
			  (+ 1 (get-value (car (reverse card-list)))))))))
	((> end-slot 1)
	 (and (not (= start-slot end-slot))
	      (or (and (empty-slot? end-slot)
		       (= (get-value (car card-list)) ace))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (get-top-card end-slot))
			  (get-suit (car card-list)))
		       (= (+ 1 (get-value (get-top-card end-slot)))
			  (get-value (car card-list)))))))
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (cond ((> end-slot 5)
	     (move-n-cards! start-slot end-slot card-list)
	     (or (> start-slot 5)
		 (= start-slot 1)
		 (add-to-score! -1)))
	    ((> end-slot 1)
	     (move-n-cards! start-slot end-slot (reverse card-list))
	     (or (> (length card-list) 1)
		 (> start-slot 5)
		 (= start-slot 1)
		 (add-to-score! -1))
	     (add-to-score! (length card-list)))
	    (#t #f))
      #f))

(define (do-deal-next-cards)
  (flip-stock 0 1 0))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (do-deal-next-cards)))

(define (dealable?)
  (flippable? 0 1 0))

(define (check-to-foundation card f-slot)
  (cond ((= f-slot 6)
	 #f)
	((and (empty-slot? f-slot)
	      (= (get-value card) ace))
	 f-slot)
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card f-slot))
		 (get-suit card)))
	 (and (= (get-value card)
		 (+ 1 (get-value (get-top-card f-slot))))
	      f-slot))
	(#t (check-to-foundation card (+ 1 f-slot)))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 1)
	   (> slot-id 5))
       (check-to-foundation (get-top-card slot-id) 2)
       (deal-cards slot-id (list (check-to-foundation (get-top-card slot-id) 2)))
       (add-to-score! 1)))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 9)
       (empty-slot? 10)
       (empty-slot? 11)
       (empty-slot? 12)))

(define (check-foundations slot)
  (cond ((= slot 13)
	 #f)
	((= slot 2)
	 (check-foundations 6))
	((and (not (empty-slot? slot))
	      (check-to-foundation (get-top-card slot) 2))
	 (hint-move slot 1 (check-to-foundation (get-top-card slot) 2)))
	(#t (check-foundations (+ 1 slot)))))

(define (check-a-tab-slot card slot2 same-suit?)
  (and (or (and (not same-suit?)
		(eq? (is-red? card)
		     (is-red? (get-top-card slot2))))
	   (= (get-suit card)
	      (get-suit (get-top-card slot2))))
       (= (+ 1 (get-value card))
	  (get-value (get-top-card slot2)))))

(define (stripped card-list slot)
  (if (or (= slot 1)
	  (= (length card-list) 1))
      (car card-list)
      (if (and (check-straight-descending-list card-list)
	       (check-same-suit-list card-list))
	  (car (reverse card-list))
	  (stripped (reverse (cdr (reverse card-list))) slot))))

(define (stripped-size card-list slot)
  (if (or (= slot 1)
	  (= (length card-list) 1))
      1
      (if (and (check-straight-descending-list card-list)
	       (check-same-suit-list card-list))
	  (length card-list)
	  (stripped-size (reverse (cdr (reverse card-list))) slot))))


(define (check-same-suit-builds slot1 slot2)
  (cond ((= slot1 13)
	 #f)
	((= slot1 2)
	 (check-same-suit-builds 6 7))
	((or (empty-slot? slot1)
	     (= slot2 13))
	 (check-same-suit-builds (+ 1 slot1) 6))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (check-a-tab-slot (stripped (get-cards slot1) slot1) slot2 #t))
	 (hint-move slot1 (stripped-size (get-cards slot1) slot1) slot2))
	(#t (check-same-suit-builds slot1 (+ 1 slot2)))))

(define (check-same-color-builds slot1 slot2)
  (cond ((= slot1 13)
	 #f)
	((= slot1 2)
	 (check-same-color-builds 6 7))
	((or (empty-slot? slot1)
	     (= slot2 13))
	 (check-same-color-builds (+ 1 slot1) 6))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (check-a-tab-slot (stripped (get-cards slot1) slot1) slot2 #f))
	 (hint-move slot1 (stripped-size (get-cards slot1) slot1) slot2))
	(#t (check-same-color-builds slot1 (+ 1 slot2)))))

(define (empty-tab? slot)
  (cond ((= slot 13)
	 #f)
	((empty-slot? slot)
	 (and (or (not (empty-slot? 1))
		  (and (not (empty-slot? 6))
		       (or (not (check-same-suit-list (get-cards 6)))
			   (not (check-straight-descending-list (get-cards 6)))))
		  (and (not (empty-slot? 7))
		       (or (not (check-same-suit-list (get-cards 7)))
			   (not (check-straight-descending-list (get-cards 7)))))
		  (and (not (empty-slot? 8))
		       (or (not (check-same-suit-list (get-cards 8)))
			   (not (check-straight-descending-list (get-cards 8)))))
		  (and (not (empty-slot? 9))
		       (or (not (check-same-suit-list (get-cards 9)))
			   (not (check-straight-descending-list (get-cards 9)))))
		  (and (not (empty-slot? 10))
		       (or (not (check-same-suit-list (get-cards 10)))
			   (not (check-straight-descending-list (get-cards 10)))))
		  (and (not (empty-slot? 11))
		       (or (not (check-same-suit-list (get-cards 11)))
			   (not (check-straight-descending-list (get-cards 11)))))
		  (and (not (empty-slot? 12))
		       (or (not (check-same-suit-list (get-cards 12)))
			   (not (check-straight-descending-list (get-cards 12))))))
	      (list 0 (G_"Move a build of cards on to the empty Tableau slot"))))
	(#t (empty-tab? (+ 1 slot)))))

(define (get-hint)
  (or (check-foundations 1)
      (check-same-suit-builds 1 6)
      (check-same-color-builds 1 6)
      (empty-tab? 6)
      (and (not (empty-slot? 0))
          (list 0 (G_"Deal another card")))))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable? dealable?)
