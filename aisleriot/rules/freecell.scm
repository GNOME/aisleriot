; freecell.scm

;; Copyright (C) 1998 Ryu Changwoo

;; This program is free software; you can redistribute it and'or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Written by Ryu Changwoo <cwryu@eve.kaist.ac.kr>


;set up the deck
(set-ace-low)


(define (new-game)
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)
  
  ;set up the board
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (set! HORIZPOS (+ 21 HORIZPOS))
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-extended-slot '() down) ; 8
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down) ; 9
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down) ; 10
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down) ; 11
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down) ; 12
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down) ; 13
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down) ; 14
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down) ; 15
  (add-blank-slot)
  (deal-initial-setup)
)
;internal procedures/variables

(define (deal-initial-setup)
  (deal-cards-face-up-from-deck DECK '(8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11))
)


;additional functions.

(define (last l)
  (list-ref l (- (length l) 1)))

(define (freecell? slot-id)
  (and (>= slot-id 0) (<= slot-id 3)))

(define (homecell? slot-id)
  (and (>= slot-id 4) (<= slot-id 7)))

(define (field? slot-id)
  (and (>= slot-id 8) (<= slot-id 15)))

(define (join? lower upper)
  (and (not (eq? (get-color lower) (get-color upper)))
       (eq? (+ (get-value lower) 1) (get-value upper))))

(define (sequence? card-list)
  (define (sequence?-iter c l)
    (if (null? l)
	#t
	(if (join? c (car l))
	    (sequence?-iter (car l) (cdr l))
	    #f)))
  (sequence?-iter (car card-list) (cdr card-list)))

(define (empty-freecell-number)
  (define (empty-freecell-number-iter n i)
    (if (> i 3)
	n
	(if (empty-slot? i)
	    (empty-freecell-number-iter (+ n 1) (+ i 1))
	    (empty-freecell-number-iter n (+ i 1)))))
  (empty-freecell-number-iter 0 0))

(define (move-to-homecell card-list homecell-id)
  (if (> (length card-list) 1)
      #f
      (let ((movingcard (car card-list))
	    (lastcard (if (empty-slot? homecell-id)
			  #f
			  (car (get-cards homecell-id)))))
	(if (eq? lastcard #f)
	    (if (eq? (get-value movingcard) 1)
		(add-card! homecell-id movingcard)
		#f)
	    (if (not (eq? (get-suit movingcard) (get-suit lastcard)))
		#f
		(if (= (get-value movingcard) (+ (get-value lastcard) 1))
		    (add-card! homecell-id movingcard)
		    #f))))))

(define (move-to-field card-list field-id)
  (if (and (sequence? card-list)
	   (<= (length card-list) (+ (empty-freecell-number) 1)))
      (if (empty-slot? field-id)
	  (add-cards! field-id card-list)
	  (let ((bottomdest (car (get-cards (get-slot field-id))))
		(topsrc (last card-list)))
	    (if (join? topsrc bottomdest)
		(add-cards! field-id card-list)
		#f)))
      #f))

(define (move-to-freecell card-list freecell-id)
  (if (> (length card-list) 1)
      #f
      (if (empty-slot? freecell-id)
	  (add-cards! freecell-id card-list)
	  #f)))

(define (button-pressed slot-id card-list)
  (cond ((homecell? slot-id) #f)
	(else (begin
		(not (empty-slot? slot-id))))))

(define (button-released start-slot-id card-list end-slot-id)
  (cond ((homecell? end-slot-id) (move-to-homecell card-list end-slot-id))
	((field? end-slot-id) (move-to-field card-list end-slot-id))
	(else (move-to-freecell card-list end-slot-id))))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (game-over ugh)
  #t)

(define (game-won ugh)
  #f)

(define (get-hint ugh)
  #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint)


 
