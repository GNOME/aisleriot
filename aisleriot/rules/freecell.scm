; freecell.scm

;; Copyright (C) 1998 Changwoo Ryu

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

;; Written by Changwoo Ryu <cwryu@adam.kaist.ac.kr>


;; set up the deck
(set-ace-low)


(define (new-game)
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)
  
  ;; set up the board

  ; freecells
  (add-normal-slot '())			; 0
  (add-normal-slot '())			; 1
  (add-normal-slot '())			; 2
  (add-normal-slot '())			; 3
  (set! HORIZPOS (+ 21 HORIZPOS))

  ; homecells
  (add-normal-slot '())			; 4
  (add-normal-slot '())			; 5
  (add-normal-slot '())			; 6
  (add-normal-slot '())			; 7
  (add-carriage-return-slot)

  ; fields
  (add-extended-slot '() down)		; 8
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down)		; 9
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down)		; 10
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down)		; 11
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down)		; 12
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down)		; 13
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down)		; 14
  (set! HORIZPOS (+ 3 HORIZPOS))
  (add-extended-slot '() down)		; 15

  (add-blank-slot)
  (deal-initial-setup)
  (list 8 3)
)

;; internal procedures/variables

(define (deal-initial-setup)
  (deal-cards-face-up-from-deck DECK '(8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11 12 13 14 15 8 9 10 11))
)


;; additional functions.

; last element from a list
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
	  (let ((bottomdest (car (get-cards field-id)))
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

; an empty freecell's id, if any
(define (any-empty-freecell)
  (cond ((empty-slot? 0) 0)
	((empty-slot? 1) 1)
	((empty-slot? 2) 2)
	((empty-slot? 3) 3)
	(else #f)))

; an empty homecell's id, if any
(define (any-empty-homecell)
  (cond ((empty-slot? 4) 4)
	((empty-slot? 5) 5)
	((empty-slot? 6) 6)
	((empty-slot? 7) 7)
	(else #f)))

; homecell id which holds the suit, if any
(define (homecell-by-suit suit)
  ; true if slot is the answer.
  (define (pred? slot)
    (and (not (empty-slot? slot))
	 (= (get-suit (get-top-card slot)) suit)))
  (cond ((pred? 4) 4)
	((pred? 5) 5)
	((pred? 6) 6)
	((pred? 7) 7)
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; auto move stuffs

(define (auto-move-to-where card)
  (define (no-joinable-card?)
    (if (= (get-color card) black)
	(let ((hid1 (homecell-by-suit diamond))
	      (hid2 (homecell-by-suit heart)))
	  (and hid1 hid2
	       (>= (get-value (get-top-card hid1)) (- (get-value card) 1))
	       (>= (get-value (get-top-card hid2)) (- (get-value card) 1))))
	(let ((hid1 (homecell-by-suit club))
	      (hid2 (homecell-by-suit spade)))
	  (and hid1 hid2
	       (>= (get-value (get-top-card hid1)) (- (get-value card) 1))
	       (>= (get-value (get-top-card hid2)) (- (get-value card) 1))))))

  (if (= (get-value card) 1)
      (any-empty-homecell)
      (let* ((suit (get-suit card))
	     (hid (homecell-by-suit suit)))
	(and hid
	     (= (get-value card) (+ (get-value (get-top-card hid)) 1))
	     (or (= (get-value card) 2) (no-joinable-card?))
	     hid))))

(define (auto-move-one-one slot)
  (if (empty-slot? slot)
      #f
      (let* ((card (get-top-card slot))
	     (to (auto-move-to-where card)))
	(and to
	     (begin (remove-card slot)
		    (move-to-homecell (list card) to)
		    #t)))))

(define (auto-move-one)
  (or (auto-move-one-one 0)
      (auto-move-one-one 1)
      (auto-move-one-one 2)
      (auto-move-one-one 3)
      (auto-move-one-one 8)
      (auto-move-one-one 9)
      (auto-move-one-one 10)
      (auto-move-one-one 11)
      (auto-move-one-one 12)
      (auto-move-one-one 13)
      (auto-move-one-one 14)
      (auto-move-one-one 15)))

(define (auto-move)
  ; cf. auto move in freecell is always monotonous.
  (and (auto-move-one) (auto-move)))

(define (button-pressed slot-id card-list)
  (cond ((homecell? slot-id) #f)
	(else (begin
		(not (empty-slot? slot-id))))))

(define (button-released start-slot-id card-list end-slot-id)
  (begin
    (cond ((homecell? end-slot-id) (move-to-homecell card-list end-slot-id))
	  ((field? end-slot-id) (move-to-field card-list end-slot-id))
	  (else (move-to-freecell card-list end-slot-id)))))
  

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  (cond ((field? slot-id)
	 ; move to an empty freecell, if it's possible
	 (let ((empty-fc (any-empty-freecell))
	       (card (get-top-card slot-id)))
	   (or (not empty-fc)
	       (null? card)
	       (begin (remove-card slot-id)
		      (add-cards! empty-fc (list card))))
	   (auto-move)))
	(else #f)))

(define (game-over)
  (not (game-won)))

(define (game-won)
  (and (= 13 (length (get-cards 4)))
       (= 13 (length (get-cards 5)))
       (= 13 (length (get-cards 6)))
       (= 13 (length (get-cards 7)))))

(define (get-hint)
  #f)
 
(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
