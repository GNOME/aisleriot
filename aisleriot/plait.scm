;;; plait.scm

;; Copyright (C) 1999, 2003 W. Borgert

;; Author: W. Borgert <debacle@debian.org>

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

;; @(#) $Id$

;; Constants
(define edge-1  0)
(define plait   1)
(define edge-2  2)
(define home-1  3)
(define home-2  4)
(define free-1  5)
(define free-2  6)
(define free-3  7)
(define free-4  8)
(define stock   9)
(define deck   10)
(define home-3 11)
(define home-4 12)
(define free-5 13)
(define free-6 14)
(define free-7 15)
(define free-8 16)
(define home-5 17)
(define home-6 18)
(define edge-3 19)
(define edge-4 20)
(define home-7 21)
(define home-8 22)
(def-save-var direc 0)
(define start-value 0)

(define up 1)
(define down 2)

;; Utilities
(define (edge? slot)
  (or (= slot edge-1)
      (= slot edge-2)
      (= slot edge-3)
      (= slot edge-4)))

(define (home? slot)
  (or (= slot home-1)
      (= slot home-2)
      (= slot home-3)
      (= slot home-4)
      (= slot home-5)
      (= slot home-6)
      (= slot home-7)
      (= slot home-8)))

(define (free? slot)
  (or (= slot free-1)
      (= slot free-2)
      (= slot free-3)
      (= slot free-4)
      (= slot free-5)
      (= slot free-6)
      (= slot free-7)
      (= slot free-8)))

;; How to move cards
(define (move-to-cell start-slot card-list end-slot)
  (and (= (length card-list) 1)
       (not (and (= start-slot plait)
		 (free? end-slot)))
       (empty-slot? end-slot)
       (add-cards! end-slot card-list)))

(define (get-valid-move slot home-list)
  (and (not (null? home-list))
       (if (and (not (empty-slot? slot))
		(move-possible? (get-top-card slot) (car home-list)))
	   (if (not (empty-slot? (car home-list)))
	       (list 1 (get-name (get-top-card slot))
		     (get-name (get-top-card (car home-list))))
	       (list 0 (format (_"Move ~a to an empty field") (get-name (get-top-card slot)))))
	   (get-valid-move slot (cdr home-list)))))

(define (get-valid-moves slot-list home-list)
 (and (not (null? slot-list))
      (or (get-valid-move (car slot-list) home-list)
	  (get-valid-moves (cdr slot-list) home-list))))

(define (deal-possible?)
  (if (not (empty-slot? deck))
      (list 0 (_"Deal a new card from the deck"))
      (if (and (< FLIP-COUNTER 2)
	       (not (empty-slot? stock)))
	  (list 0 (_"Move waste back to stock"))
	  #f)))

(define (move-upwards-possible? top-card-value new-card-value)
  (and (not (eq? direc down))
       (or (= top-card-value
	      (- new-card-value 1))
	   (and (= top-card-value king)
		(= new-card-value ace)))))

(define (move-downwards-possible? top-card-value new-card-value)
  (and (not (eq? direc up))
       (or (= top-card-value
	      (+ new-card-value 1))
	   (and (= top-card-value ace)
		(= new-card-value king)))))

(define (move-possible? card end-slot)
  (and (< (length (get-cards end-slot)) 13)
       (or (empty-slot? end-slot)
	   (and (= (get-suit (car (get-cards end-slot)))
		   (get-suit card))
		(or (move-upwards-possible?
		     (get-value (car (get-cards end-slot)))
		     (get-value card))
		    (move-downwards-possible?
		     (get-value (car (get-cards end-slot)))
		     (get-value card)))))
       (or (not (empty-slot? end-slot))
	   (= (get-value card) start-value))))

(define (move-to-home card-list end-slot)
  (if (and (= (length card-list) 1)
	   (move-possible? (car card-list) end-slot))
      (begin
	(if (and (eq? direc 0)
		 (not (empty-slot? end-slot)))
 	    (let ((es (get-value (car (get-cards end-slot))))
 		  (cl (get-value (car card-list))))
	      (if (or (and (< es cl) (not (and (= es ace) (= cl king))))
		      (and (= es king) (= cl ace)))
		(set! direc up)
		(set! direc down))))
	(add-cards! end-slot card-list))
      #f))

;; find the center between two slots horizontally
(define (get-and-increment-position-half)
  (let ((retval (list HORIZPOS VERTPOS)))
    (set! HORIZPOS (+ HORIZPOS 0.5))
    retval))

;; Set up a new game.
(define (new-game)
  (initialize-playing-area)
  (make-standard-double-deck)
  (shuffle-deck)

  (get-and-increment-position-half)
  (add-normal-slot '())
  (add-blank-slot)
  (add-extended-slot '() down)
  (add-blank-slot)
  (add-normal-slot '())
  (get-and-increment-position-half)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (set! VERTPOS (+ VERTPOS 0.5))
  (add-normal-slot '())
  (add-normal-slot DECK)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (get-and-increment-position-half)
  (add-normal-slot '())
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (get-and-increment-position-half)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (deal-cards-face-up deck '(0
			     1 1 1 1 1 1 1 1 1 1
			     1 1 1 1 1 1 1 1 1 1
			     2 3 5 6 7 8 13 14 15 16 19 20))
  (set! direc 0)
  (set! start-value (get-value (get-top-card home-1)))

  (list 10 4))

;; Move cards automatically from the plait to one of the edge slots
(define (plait-to-edge start-slot)
  (if (and (edge? start-slot)
	   (not (empty-slot? plait)))
      (let ((top-card (remove-card plait)))
	(add-card! start-slot top-card))
  #t))


(define (button-pressed slot card-list)
  (if (or (free? slot)
	  (edge? slot)
	  (= stock slot)
	  (= plait slot))
      #t
      #f))

(define (button-released start-slot card-list end-slot)
  (cond ((free? end-slot) (move-to-cell start-slot card-list end-slot))
	((home? end-slot) (and
			    (move-to-home card-list end-slot)
			    (plait-to-edge start-slot)))
	((edge? end-slot) (move-to-cell start-slot card-list end-slot))
	(else #f)))
  
(define (button-clicked slot)
  (if (or (= slot deck)
	  (= slot stock))
      (flip-stock deck stock 2)
      #f))

;; TODO: Automatically move all possible cards on double-click
(define (button-double-clicked slot) #f)

;; Is there something to do?
(define (game-cont)
  (and (not (game-won))
       (or (deal-possible?)
	   (get-valid-moves '(1 0 2 19 20 9 5 6 7 8 13 14 15 16)
			    '(3 4 11 12 17 18 21 22)))))

;; Condition for win -- all the cards in homecells
(define (game-won)
  (and (= 13 (length (get-cards home-1)))
       (= 13 (length (get-cards home-2)))
       (= 13 (length (get-cards home-3)))
       (= 13 (length (get-cards home-4)))
       (= 13 (length (get-cards home-5)))
       (= 13 (length (get-cards home-6)))
       (= 13 (length (get-cards home-7)))
       (= 13 (length (get-cards home-8)))))

(define (get-hint)
  (or (get-valid-moves '(1 0 2 19 20 9 5 6 7 8 13 14 15 16)
		       '(3 4 11 12 17 18 21 22))
      (deal-possible?)
      (list 0 (_"Try a new game"))))
 
(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked
	    button-double-clicked game-cont game-won get-hint
	    get-options apply-options timeout)

;;; plait.scm ends here
