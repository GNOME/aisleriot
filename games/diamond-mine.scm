; AisleRiot - diamond_mine.scm
; Copyright (C) 2001, 2003 Rosanna Yuen <zana@webwynk.net>
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

(define foundation 0)
(define tableau '(1 2 3 4 5 6 7 8 9 10 11 12 13))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot DECK 'foundation)
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
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (deal-cards 0 '(1 2 3 4 5 6 7 8 9 10 11 12 13 1 2 3 4 5 6 7 8 9 10
		    11 12 13 1 2 3 4 5 6 7 8 9 10 11 12 13))


  (deal-cards-face-up 0 '(1 2 3 4 5 6 7 8 9 10 11 12 13))

  
  (list 13 4))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)
       (is-visible? (car (reverse card-list)))))

(define (droppable? start-slot card-list end-slot)
  (cond
   ((= start-slot end-slot) #f)
   ((= end-slot 0)
    (and (= (length card-list) 1)
	 (= (get-suit (car card-list)) diamond)
	 (or (empty-slot? 0)
	     (= (modulo (get-value (car card-list)) 13)
		(modulo (+ 1 (get-value (get-top-card 0))) 13)))))
   (#t
    (and (not (= (get-suit (car card-list)) diamond))
	 (or (empty-slot? end-slot)
	     (and (not (= (get-suit (get-top-card end-slot)) diamond))
		  (= (get-value (get-top-card end-slot))
		     (+ 1 (get-value (car (reverse card-list)))))))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (begin
	 (move-n-cards! start-slot end-slot card-list)
	 (if (= (get-suit (car card-list)) diamond)
	     (add-to-score! (get-value (car card-list)))
	     (begin
	       (and (= (length (get-cards end-slot)) 13)
		    (check-same-suit-list (get-cards end-slot))
		    (add-to-score! 3))
	       (and (= 13 (+ (length (get-cards start-slot))
			     (length card-list)))
		    (check-same-suit-list card-list)
		    (check-same-suit-list (get-cards start-slot))
		    (or (= (length (get-cards start-slot)) 0)
			(= (get-suit (get-top-card start-slot))
			   (get-suit (car card-list))))
		    (add-to-score! -3))))
	 (or (empty-slot? start-slot)
	     (make-visible-top-card start-slot)))))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)
       (= (get-suit (get-top-card slot-id)) diamond)
       (or (and (empty-slot? 0)
		(deal-cards slot-id '(0))
		(or (empty-slot? slot-id)
		    (make-visible-top-card slot-id))
		(add-to-score! (get-value (get-top-card 0))))
	   (and (or (= (get-value (get-top-card slot-id))
		       (+ 1 (get-value (get-top-card 0))))
		    (and (= (get-value (get-top-card slot-id)) ace)
			 (= (get-value (get-top-card 0)) king)))
		(deal-cards slot-id '(0))
		(or (empty-slot? slot-id)
		    (make-visible-top-card slot-id))
		(add-to-score! (get-value (get-top-card 0)))))))


(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (check-slots-for-win slot)
  (cond ((= slot 14)
	 #t)
	((or (empty-slot? slot)
	     (and (= (length (get-cards slot)) 13)
		  (check-same-suit-list (get-cards slot))
		  (is-visible? (car (reverse (get-cards slot))))))
	 (check-slots-for-win (+ 1 slot)))
	(#t #f)))

(define (game-won)
  (and (= (length (get-cards 0)) 13)
       (check-slots-for-win 1)))

(define (check-to-foundation slot)
  (cond ((or (empty-slot? 0)
	     (= slot 14))
	 #f)
	((and (not (empty-slot? slot))
	      (= (get-suit (get-top-card slot)) diamond)
	      (or (= (get-value (get-top-card slot))
		     (+ 1 (get-value (get-top-card 0))))
		  (and (= (get-value (get-top-card slot)) ace)
		       (= (get-value (get-top-card 0)) king))))
	 (hint-move slot 1 0))
	(#t (check-to-foundation (+ 1 slot)))))

(define (stripped card-list card)
  (cond ((>= (get-value (car card-list))
	     (get-value card))
	 (car card-list))
	((< (length card-list) 2)
	 (car card-list))
	((= (+ 1 (get-value (car card-list)))
	    (get-value card))
	 (car card-list))
	((is-visible? (cadr card-list))
	 (stripped (cdr card-list) card))
	(#t (car card-list))))

(define (stripped-size card-list card size)
  (cond ((>= (get-value (car card-list))
	     (get-value card))
	 size)
	((< (length card-list) 2)
	 size)
	((= (+ 1 (get-value (car card-list)))
	    (get-value card))
	 size)
	((is-visible? (cadr card-list))
	 (stripped-size (cdr card-list) card (+ size 1)))
	(#t size)))


(define (check-same-suit-build slot1 slot2)
  (cond ((= slot1 14)
	 #f)
	((or (empty-slot? slot1)
	     (= (get-suit (get-top-card slot1)) diamond)
	     (= slot2 14))
	 (check-same-suit-build (+ 1 slot1) 1))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (not (= (get-suit (get-top-card slot2))
		      diamond))
	      (= (get-suit (stripped (get-cards slot1) 
				     (get-top-card slot2)))
		 (get-suit (get-top-card slot2)))
	      (= (+ 1 (get-value (stripped (get-cards slot1) 
					   (get-top-card slot2))))
		 (get-value (get-top-card slot2))))
	 (hint-move slot1 (stripped-size (get-cards slot1) (get-top-card slot2) 1) slot2))
	(#t 
	 (check-same-suit-build slot1 (+ 1 slot2)))))

(define (uncover? card-list card)
  (if (not (is-visible? (car (reverse card-list))))
      (uncover? (reverse (cdr (reverse card-list))) card)
      (and (= (get-value (car (reverse card-list)))
	      (get-value card))
	   (= (get-suit (car (reverse card-list)))
	      (get-suit card)))))

(define (check-diff-suit-build slot1 slot2)
  (cond ((= slot1 14)
	 #f)
	((or (empty-slot? slot1)
	     (= (get-suit (get-top-card slot1)) diamond)
	     (= slot2 14))
	 (check-diff-suit-build (+ 1 slot1) 1))
	((and (not (= slot1 slot2))
	      (not (empty-slot? slot2))
	      (not (= (get-suit (get-top-card slot2))
		      diamond))
	      (= (+ 1 (get-value (stripped (get-cards slot1) 
					   (get-top-card slot2))))
		 (get-value (get-top-card slot2)))
	      (uncover? (get-cards slot1) 
			(stripped (get-cards slot1)
				  (get-top-card slot2))))
	 (hint-move slot1 (stripped-size (get-cards slot1) (get-top-card slot2) 1) slot2))
	(#t 
	 (check-diff-suit-build slot1 (+ 1 slot2)))))

(define (simple-strip card-list)
  (if (not (is-visible? (car (reverse card-list))))
      (simple-strip (reverse (cdr (reverse card-list))))
      (length card-list)))

(define (possible-move-off? slot dest-slot)
  (cond ((= slot 14)
	 #f)
	((and (not (empty-slot? slot))
	      (not (is-visible? (car (reverse (get-cards slot)))))
	      (not (= (get-suit (get-top-card slot)) diamond)))
	 (hint-move slot (simple-strip (get-cards slot)) dest-slot))
	(#t (possible-move-off? (+ 1 slot) dest-slot))))

(define (check-for-empties slot)
  (if (= slot 14)
      #f
      (or (and (empty-slot? slot)
	       (possible-move-off? 0 slot))
          (check-for-empties (+ 1 slot)))))

(define (start-foundation slot)
  (cond ((or (not (empty-slot? 0))
	     (= slot 14))
	 #f)
	((and (not (empty-slot? slot))
	      (= (get-suit (get-top-card slot)) diamond))
	 (hint-move slot 1 0))
	(#t (start-foundation (+ 1 slot)))))

(define (any-empty? slot)
  (cond ((= slot 14)
	 #f)
	((empty-slot? slot)
	 #t)
	(#t (any-empty? (+ 1 slot)))))

(define (search-a-slot card-suit card-rank card-list)
  (cond ((and (= (get-suit (car card-list)) card-suit)
	      (= (get-value (car card-list)) card-rank))
	 (is-visible? (car card-list)))
	((> (length card-list) 1)
	 (search-a-slot card-suit card-rank (cdr card-list)))
	(#t #f)))

(define (visible-on-tableau? card-suit card-rank slot)
  (cond ((= slot 14)
	 #f)
	((and (not (empty-slot? slot))
	      (search-a-slot card-suit card-rank (get-cards slot)))
	 #t)
	(#t (visible-on-tableau? card-suit card-rank (+ 1 slot)))))

(define (check-a-tab-slot card-list slot)
  (cond ((or (< (length card-list) 2)
	     (not (is-visible? (cadr card-list))))
	 #f)
	((and (not (= (get-suit (car card-list))
		      (get-suit (cadr card-list))))
	      (visible-on-tableau? (get-suit (car card-list)) (+ 1 (get-value (car card-list))) 1))
	 slot)
	(#t (check-a-tab-slot (cdr card-list) (+ slot 1)))))

(define (check-tableau-suit-changes slot)
  (cond ((or (= slot 14)
	     (not (any-empty? 1)))
	 #f)
	((and (not (empty-slot? slot))
	      (check-a-tab-slot (get-cards slot) 1))
	 (hint-move slot (check-a-tab-slot (get-cards slot) 1) (find-empty-slot tableau)))
	(#t (check-tableau-suit-changes (+ 1 slot)))))

(define (get-hint)
  (or (check-to-foundation 1)
      (check-same-suit-build 1 2)
      (check-diff-suit-build 1 2)
      (check-for-empties 1)
      (start-foundation 1)
      (check-tableau-suit-changes 1)))

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
