;;; freecell.scm -- Free Cell game for AisleRiot.

;; Copyright (C) 1998 Changwoo Ryu

;; Author: Changwoo Ryu <cwryu@adam.kaist.ac.kr>

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

;;; Commentary:

;; FREECELL
;;
;; * The 4 slots in the left-top are called "freecells". (F? in the below)
;; * The 4 slots in the right-top are called "homecells". (H? in the below)
;; * The 8 slots in the bottom are called "fields". (D? in the below)
;;
;;  -------------------------------------------
;;  |                                         |
;;  |(0)  (1)  (2)  (3)    (4)  (5)  (6)  (7) |
;;  | F1   F2   F3   F4     H1   H2   H3   H4 |
;;  |                                         |
;;  |                                         |
;;  | (8)  (9)  (10) (11) (12) (13) (14) (15) |
;;  |  D1   D2   D3   D4   D5   D6   D7   D8  |
;;  |                                         |
;;  -------------------------------------------

;;; Code:

;;
;; Game Options
;;
(define option-auto-move #t)
(define option-one-by-one #f)

;;
;; Constants
;;
(define freecell-1 0)
(define freecell-2 1)
(define freecell-3 2)
(define freecell-4 3)
(define homecell-1 4)
(define homecell-2 5)
(define homecell-3 6)
(define homecell-4 7)
(define field-1    8)
(define field-2    9)
(define field-3    10)
(define field-4    11)
(define field-5    12)
(define field-6    13)
(define field-7    14)
(define field-8    15)

;;
;; Initial cards
;;
(define (deal-initial-setup)
  (let ((fields (list field-1 field-2 field-3 field-4
			       field-5 field-6 field-7 field-8))
	(half-fields (list field-1 field-2 field-3 field-4)))
    (deal-cards-face-up-from-deck DECK
				  (append fields fields fields
					  fields fields fields
					  half-fields))))

;;
;; Utilities
;;

(define (freecell? slot)
  (and (>= slot freecell-1) (<= slot freecell-4)))

(define (homecell? slot)
  (and (>= slot homecell-1) (<= slot homecell-4)))

(define (field? slot)
  (and (>= slot field-1) (<= slot field-8)))

(define (slot-type slot)
  (cond ((freecell? slot) 'freecell)
	((homecell? slot) 'homecell)
	((field? slot) 'field)))

(define (opposite-color color)
  (if (eq? color red) black red))
      

;;
;; Utilities for the homecells
;;

;; homecell id which holds the suit.  #f if no such homecell exists.
(define (homecell-by-suit suit)
  (define (p? slot)
    (and (not (empty-slot? slot))
	 (= (get-suit (get-top-card slot)) suit)))
  (cond ((p? homecell-1) homecell-1)
	((p? homecell-2) homecell-2)
	((p? homecell-3) homecell-3)
	((p? homecell-4) homecell-4)
	(else #f)))

;; An empty homecell's id, if any
(define (any-empty-homecell)
  (cond ((empty-slot? homecell-1) homecell-1)
	((empty-slot? homecell-2) homecell-2)
	((empty-slot? homecell-3) homecell-3)
	((empty-slot? homecell-4) homecell-4)
	(else #f)))

(define (homecell-join? prev next)
  (and (eq? (get-suit prev) (get-suit next))
       (eq? (+ (get-value prev) 1) (get-value next))))

(define (get-color-homecells color)
  (define (iter n l)
    (if (< n homecell-1)
	l
	(if (eq? (get-top-card n) color)
	    (iter (- n 1) (cons n l))
	    (iter (- n 1) l))))
  (iter homecell-4 '()))

;;
;; Utilities for freecells
;;

;; The total number of empty freecells
(define (empty-freecell-number)
  (do ((i freecell-1 (+ i 1))
       (sum 0 (+ sum (if (empty-slot? i) 1 0))))
      ((> i freecell-4) sum)))

;; An empty freecell's id, if any
(define (any-empty-freecell)
  (cond ((empty-slot? freecell-1) freecell-1)
	((empty-slot? freecell-2) freecell-2)
	((empty-slot? freecell-3) freecell-3)
	((empty-slot? freecell-4) freecell-4)
	(else #f)))

;;
;; Utilities for fields
;;

(define (field-join? lower upper)
  (and (not (eq? (get-color lower) (get-color upper)))
       (eq? (+ (get-value lower) 1) (get-value upper))))

(define (field-sequence? card-list)
  (or (null? card-list)
      (null? (cdr card-list))
      (and (field-join? (car card-list) (cadr card-list))
	   (field-sequence? (cdr card-list)))))

;;
;; How to move cards
;;

(define (move-to-homecell card-list homecell-id)
  (and (= (length card-list) 1)
       (let ((card (car card-list)))
	 (if (empty-slot? homecell-id)
	     ; Put an Ace into an empty homecell
	     (and (eq? (get-value card) ace)
		  (add-to-score! 1)
		  (add-card! homecell-id card))
	     ; Put a +1 card into the homecell, whose suit is same.
	     (and (homecell-join? (car (get-cards homecell-id)) card)
		  (add-to-score! 1)
		  (add-card! homecell-id card))))))

(define (move-to-field card-list field-id)
  (and (field-sequence? card-list)
       (<= (length card-list) (+ (empty-freecell-number) 1))
       (if (empty-slot? field-id)
	   (add-cards! field-id card-list)
	   (let ((dest-top (car (get-cards field-id))))
	     (and (field-sequence? (append card-list (list dest-top)))
		  (add-cards! field-id card-list))))))

(define (move-to-freecell card-list freecell-id)
  (and (= (length card-list) 1)
       (empty-slot? freecell-id)
       (add-cards! freecell-id card-list)))

;;
;; Auto move stuffs
;;


;;
;; Callbacks & Initialize the game
;;

;; Set up a new game.
(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
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

(define (button-pressed slot card-list)
  (cond ((homecell?   slot) #f)
	((field?      slot) (field-sequence? card-list))
	((freecell?   slot) #t)))

(define (button-released start-slot card-list end-slot)
  (cond ((homecell? end-slot) (move-to-homecell card-list end-slot))
	((field?    end-slot) (move-to-field    card-list end-slot))
	((freecell? end-slot) (move-to-freecell card-list end-slot))))
  
(define (button-clicked slot)
  ; (FIXME)
  #f)

(define (button-double-clicked slot)
  (and (field? slot)
       (not (empty-slot? slot))
       (let ((card (get-top-card slot)))
	 (if (eq? (get-value card) ace)
	     ; move the Ace to an empty homecell
	     (begin (remove-card slot)
		    (add-card! (any-empty-homecell) card))
	     ; move if the same suit and -1 value card is in a homecell.
	     ; (FIXME)
	     #f))))

;; Condition for fail -- no more cards to move
(define (game-over)
  ; (FIXME)
  (not (game-won)))

;; Condition for win -- all the cards in homecells
(define (game-won)
  (and (= 13 (length (get-cards homecell-1)))
       (= 13 (length (get-cards homecell-2)))
       (= 13 (length (get-cards homecell-3)))
       (= 13 (length (get-cards homecell-4)))))

(define (get-hint)
  ; (FIXME)
  #f)
 
(define (get-options) 
  #f)
;  '(("Auto move to homecell" option-auto-move)
;    ("Move one by one" option-one-by-one)))

(define (apply-options options) 
  #f)
;  (set! option-auto-move (cadar options))
;  (set! option-auto-move (cadadr options)))

(define (timeout) 
  ; (FIXME)
  #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)

;;; freecell.scm ends here
