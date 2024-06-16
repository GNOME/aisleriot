; AisleRiot - bakers_game.scm
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

(def-save-var free-reserves 0)

(define foundation '(0 1 2 3))
(define reserve '(4 5 6 7 8 9 10 11))
(define tableau '(12 13 14 15 16 17 18 19))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'foundation)
  (add-carriage-return-slot)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-normal-slot '() 'foundation)


  (set! VERTPOS 0)

  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-normal-slot '() 'reserve) ; 4
  (add-normal-slot '() 'reserve)
  (add-normal-slot '() 'reserve)
  (add-normal-slot '() 'reserve)
  (add-normal-slot '() 'reserve)
  (add-normal-slot '() 'reserve)
  (add-normal-slot '() 'reserve)
  (add-normal-slot '() 'reserve) ; 11

  (add-carriage-return-slot)

  (add-blank-slot)
  (set! HORIZPOS (+ HORIZPOS 0.5))

  (add-extended-slot '() down 'tableau) ; 12
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau) ; 19

  (deal-cards-face-up 0 '(12 13 14 15 16 17 18 19 12 13 14 15 16 17 18
			     19 12 13 14 15 16 17 18 19 12 13 14 15 16
			     17 18 19 12 13 14 15 16 17 18 19 12 13 14 
			     15 16 17 18 19 4 5 6 7))

  (set! free-reserves 4)

  (list 9.5 4))

(define (button-pressed slot-id card-list)
  (and (> slot-id 3)
       (not (empty-slot? slot-id))
       (or (= (length card-list) 1)
	   (and (> slot-id 11)
		(< (length card-list) (+ 2 free-reserves))
		(check-same-suit-list card-list)
		(check-straight-descending-list card-list)))))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((> end-slot 11)
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
	      (> end-slot 3)
	      (< end-slot 12))
	 #t)
	((and (= (length card-list) 1)
	      (< end-slot 4))
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
       (or (< start-slot 4)
           (> start-slot 11)
           (set! free-reserves (+ free-reserves 1)))
       (or (< end-slot 4)
           (> end-slot 11)
           (set! free-reserves (- free-reserves 1)))
       (or (> start-slot 3)
           (add-to-score! -1))
       (or (> end-slot 3)
           (add-to-score! 1))
       (move-low-cards 0)))

(define (button-clicked slot-id)
  #f)

(define (move-to-empty-foundation slot f-slot)
  (cond ((= f-slot 4)
	 #f)
	((empty-slot? f-slot)
	 (deal-cards slot (list f-slot)))
	(#t (move-to-empty-foundation slot (+ 1 f-slot)))))

(define (move-to-foundation slot f-slot)
  (cond ((= f-slot 4)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card f-slot))))
	 (and (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot))))
	      (deal-cards slot (list f-slot))))
	(#t (move-to-foundation slot (+ 1 f-slot)))))

(define (move-low-cards slot-id)
  (or
    (and
     (> slot-id 3)
     (not (empty-slot? slot-id))
     (or (and (= (get-value (get-top-card slot-id))
                ace)
             (move-to-empty-foundation slot-id 0))
        (move-to-foundation slot-id 0))
     (add-to-score! 1)
     (or (> slot-id 11)
        (set! free-reserves (+ 1 free-reserves)))
     (delayed-call ((lambda (x) (lambda () (move-low-cards x))) 0))
    )
    (if (< slot-id 19)
        (move-low-cards (+ 1 slot-id))
        #t
        )
    )
  )

(define (any-empty-reserve)
  (cond
    ((empty-slot? 4) 4)
    ((empty-slot? 5) 5)
    ((empty-slot? 6) 6)
    ((empty-slot? 7) 7)
    ((empty-slot? 8) 8)
    ((empty-slot? 9) 9)
    ((empty-slot? 10) 10)
    ((empty-slot? 11) 11)
    (else #f)))

(define (move-card-to-reserve card reserve-id)
    (and
        (not (boolean? reserve-id))
        (empty-slot? reserve-id)
        (add-card! reserve-id card)
        (set! free-reserves (- free-reserves 1))
    )
)

(define (button-double-clicked slot-id)
  (or
    ; After setup, the reserve may have an ace. Allow double-clicking it.
    (and
      (> slot-id 3)
	  (< slot-id 12)
      (not (empty-slot? slot-id))
	  (= (get-value (get-top-card slot-id)) ace)
      (move-low-cards 0))
    (and
      (> slot-id 11)
      (not (empty-slot? slot-id))
      (let ((card (get-top-card slot-id)))
        (and
          (move-card-to-reserve card (any-empty-reserve))
          (remove-card slot-id)
          (move-low-cards 0)
        )
      )
    )
  )
)

(define (game-continuable)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 0)) 13)
       (= (length (get-cards 1)) 13)
       (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)))

(define (check-to-foundations? slot f-slot)
  (cond ((= slot 20)
	 #f)
	((or (empty-slot? slot)
	     (= f-slot 4))
	 (check-to-foundations? (+ 1 slot) 0))
	((= (get-value (get-top-card slot)) ace)
	 (hint-move slot 1 (find-empty-slot foundation)))
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (hint-move slot 1 f-slot))
	(#t (check-to-foundations? slot (+ 1 f-slot)))))

(define (check-for-king card-list iter slot)
  (cond ((= (length card-list) 0)
	 #f)
	((and (= (length card-list) 1)
	      (> slot 11))
	 #f)
	((= (get-value (car card-list)) king)
	 #t)
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

(define (check-to-tableau? slot t-slot)
  (cond ((= slot 20)
	 #f)
	((or (empty-slot? slot)
	     (= t-slot 20))
	 (check-to-tableau? (+ 1 slot) 12))
	((and (not (= slot t-slot))
	      (empty-slot? t-slot)
	      (check-for-king (get-cards slot) free-reserves slot))
	 (hint-move slot (- 14 (get-value (get-top-card slot))) t-slot))
	((and (not (= slot t-slot))
	      (not (empty-slot? t-slot))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card t-slot)))
	      (check-for-spec-card (get-cards slot) 
				   free-reserves 
				   (- (get-value (get-top-card t-slot)) 1)))
	 (hint-move slot (- (get-value (get-top-card t-slot)) (get-value (get-top-card slot))) t-slot))
	(#t (check-to-tableau? slot (+ 1 t-slot)))))

(define (check-for-empty-reserve)
  (and (> free-reserves 0)
       (list 0 (G_"Move something on to an empty reserve"))))

(define (get-hint)
  (or (check-to-foundations? 4 0)
      (check-to-tableau? 4 12)
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
