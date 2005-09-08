; AisleRiot - bakers_game.scm
; Copyright (C) 2001, 2003 Rosanna Yuen <zana@webwynk.net>
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

(def-save-var free-reserves 0)

(define reserve '(0 1 2 3))
(define foundation '(4 5 6 7))
(define tableau '(8 9 10 11 12 13 14 15))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)

  (set! HORIZPOS 0.5)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (deal-cards-face-up 0 '(8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15
			    8 9 10 11 12 13 14 15 8 9 10 11 12 13 14
			    15 8 9 10 11 12 13 14 15 8 9 10 11 12 13
			    14 15 8 9 10 11))

  (set! free-reserves 4)

  (list 9 4))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (or (= (length card-list) 1)
	   (and (member slot-id tableau)
		(< (length card-list) (+ 2 free-reserves))
		(check-same-suit-list card-list)
		(check-straight-descending-list card-list)))))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((member end-slot tableau)
	 (and (or (and (empty-slot? end-slot)
		       (= (get-value (car (reverse card-list)))
			  king))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (get-top-card end-slot))
			  (get-suit (car card-list)))
		       (= (get-value (get-top-card end-slot))
			  (+ 1 (get-value (car (reverse card-list)))))))))
	((and (= (length card-list) 1)
	      (empty-slot? end-slot)
	      (member end-slot reserve))
	 #t)
	((and (= (length card-list) 1)
	      (member end-slot foundation))
	 (and (or (and (empty-slot? end-slot)
		       (= (get-value (car card-list)) ace))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (get-top-card end-slot))
			  (get-suit (car card-list)))
		       (= (+ 1 (get-value (get-top-card end-slot)))
			  (get-value (car card-list)))))))
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (or (not (member start-slot reserve))
           (set! free-reserves (+ free-reserves 1)))
       (or (not (member end-slot reserve))
           (set! free-reserves (- free-reserves 1)))
       (or (not (member start-slot foundation))
           (add-to-score! -1))
       (or (not (member end-slot foundation))
           (add-to-score! 1))))

(define (button-clicked slot-id)
  #f)

(define (move-to-empty-foundation slot f-slots)
  (cond ((eq? f-slots '())
	 #f)
	((empty-slot? (car f-slots))
	 (deal-cards slot (list (car f-slots))))
	(#t (move-to-empty-foundation slot (cdr f-slots)))))

(define (move-to-foundation slot f-slots)
  (cond ((eq? f-slots '())
	 #f)
	((and (not (empty-slot? (car f-slots)))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card (car f-slots)))))
	 (and (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card (car f-slots)))))
	      (deal-cards slot (list (car f-slots)))))
	(#t (move-to-foundation slot (cdr f-slots)))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (or (member slot-id reserve)
	   (member slot-id tableau))
       (or (and (= (get-value (get-top-card slot-id))
		   ace)
		(move-to-empty-foundation slot-id foundation))
	   (move-to-foundation slot-id foundation))
       (add-to-score! 1)
       (or (member slot-id tableau)
	   (set! free-reserves (+ 1 free-reserves)))))

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (check-full f-slots)
  (or (eq? f-slots '())
      (and (= (length (get-cards (car f-slots))) 13)
           (check-full (cdr f-slots)))))

(define (game-won)
  (check-full foundation))

(define (check-to-foundations? slots f-slots)
  (cond ((eq? slots '())
         #f)
        ((or (empty-slot? (car slots))
             (eq? f-slots '()))
         (check-to-foundations? (cdr slots) foundation))
	((= (get-value (get-top-card (car slots))) ace)
	 (list 2 (get-name (get-top-card (car slots))) (_"an empty foundation")))
	((and (not (empty-slot? (car f-slots)))
	      (= (get-suit (get-top-card (car slots)))
		 (get-suit (get-top-card (car f-slots))))
	      (= (get-value (get-top-card (car slots)))
		 (+ 1 (get-value (get-top-card (car f-slots))))))
	 (list 1 (get-name (get-top-card (car slots))) (get-name (get-top-card (car f-slots)))))
        (#t (check-to-foundations? slots (cdr f-slots)))))

(define (check-for-king card-list iter slot)
  (cond ((= (length card-list) 0)
	 #f)
	((and (= (length card-list) 1)
	      (member slot tableau))
	 #f)
	((= (get-value (car card-list)) king)
	 (get-name (car card-list)))
	((= iter 0)
	 #f)
	((and (> (length card-list)1)
	      (= (get-suit (car card-list))
		 (get-suit (cadr card-list)))
	      (= (+ 1 (get-value (car card-list)))
		 (get-value (cadr card-list))))
	 (check-for-king (cdr card-list) (- iter 1) slot))
	(#t #f)))

(define (check-for-spec-card card-list iter value)
  (cond ((= (length card-list) 0)
	 #f)
	((= (get-value (car card-list)) value)
	 #t)
	((= iter 0)
	 #f)
	((and (> (length card-list) 1)
	      (= (get-suit (car card-list))
		 (get-suit (cadr card-list)))
	      (= (+ 1 (get-value (car card-list)))
		 (get-value (cadr card-list))))
	 (check-for-spec-card (cdr card-list) (- iter 1) value))
	(#t #f)))

(define (check-to-tableau? slots t-slots)
  (cond ((eq? slots '())
         #f)
        ((or (empty-slot? (car slots))
             (eq? t-slots '()))
         (check-to-tableau? (cdr slots) tableau))
        ((= (car slots) (car t-slots))
         (check-to-tableau? slots (cdr t-slots)))
	((and (empty-slot? (car t-slots))
	      (check-for-king (get-cards (car slots)) free-reserves (car slots)))
	 (list 2 
	       (check-for-king (get-cards (car slots)) free-reserves (car slots)) 
	       (_"an empty tableau")))
	((and (not (empty-slot? (car t-slots)))
	      (= (get-suit (get-top-card (car slots)))
		 (get-suit (get-top-card (car t-slots))))
	      (check-for-spec-card (get-cards (car slots))
				   free-reserves 
				   (- (get-value (get-top-card (car t-slots))) 1)))
	 (list 1 
	       (get-name (make-card (- (get-value (get-top-card (car t-slots))) 1)
				    (get-suit (get-top-card (car t-slots)))))
	       (get-name (get-top-card (car t-slots)))))
        (#t (check-to-tableau? slots (cdr t-slots)))))

(define (check-for-empty-reserve)
  (and (> free-reserves 0)
       (list 0 (_"Move something on to an empty reserve"))))

(define (get-hint)
  (or (check-to-foundations? (append reserve tableau) foundation)
      (check-to-tableau? (append reserve tableau) tableau)
      (check-for-empty-reserve)))

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
