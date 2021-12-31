;;; plait.scm

;; Copyright (C) 1999, 2003 W. Borgert

;; Author: W. Borgert <debacle@debian.org>

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

;; @(#) $Id: plait.scm,v 1.14 2005/07/10 07:08:54 callum Exp $

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
           (hint-move slot 1 (car home-list))
	   (get-valid-move slot (cdr home-list)))))

(define (get-valid-moves slot-list home-list)
 (and (not (null? slot-list))
      (or (get-valid-move (car slot-list) home-list)
	  (get-valid-moves (cdr slot-list) home-list))))

(define (deal-possible?)
  (if (not (empty-slot? deck))
      (list 0 (G_"Deal a new card from the deck"))
      (if (and (< FLIP-COUNTER 2)
	       (not (empty-slot? stock)))
	  (list 0 (G_"Move waste back to stock"))
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
  (add-normal-slot '() 'edge)
  (add-blank-slot)
  (add-extended-slot '() down 'plait)
  (add-blank-slot)
  (add-normal-slot '() 'edge)
  (get-and-increment-position-half)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (set! VERTPOS (+ VERTPOS 0.5))
  (add-normal-slot '() 'waste)
  (add-normal-slot DECK 'stock)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'tableau)
  (add-normal-slot '() 'tableau)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (get-and-increment-position-half)
  (add-normal-slot '() 'edge)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'edge)
  (get-and-increment-position-half)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (deal-cards-face-up deck '(0
			     1 1 1 1 1 1 1 1 1 1
			     1 1 1 1 1 1 1 1 1 1
			     2 3 5 6 7 8 13 14 15 16 19 20))
  (set! direc 0)
  (set! start-value (get-value (get-top-card home-1)))

  (list 10 4))


(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-base-string)
					"   "
					(get-redeals-string))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards deck)))))

(define (get-base-string)
  (cond ((and (> start-value 1)
	      (< start-value 11))
	 (string-append (G_"Base Card: ") (number->string start-value)))
	((= start-value 1)
	 (G_"Base Card: Ace"))
	((= start-value 11)
	 (G_"Base Card: Jack"))
	((= start-value 12)
	 (G_"Base Card: Queen"))
	((= start-value 13)
	 (G_"Base Card: King"))
	(#t "")))

(define (get-redeals-string)
  (string-append (G_"Redeals left:") " "
		 (number->string (- 2 FLIP-COUNTER))))

;; Move cards automatically from the plait to one of the edge slots
(define (plait-to-edge start-slot)
  (if (and (edge? start-slot)
	   (not (empty-slot? plait)))
      (let ((top-card (remove-card plait)))
	(add-card! start-slot top-card))
  #t))


(define (button-pressed slot card-list)
  (and (or (free? slot)
           (edge? slot)
           (= stock slot)
           (= plait slot))
       (= (length card-list) 1)))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot) #f)
        ((or (free? end-slot) (edge? end-slot)) (and
						 (= (length card-list) 1)
						 (not (and (= start-slot plait)
							   (free? end-slot)))
						 (empty-slot? end-slot)))
	((home? end-slot) (move-possible? (car card-list) end-slot))
	(else #f)))

(define (button-released start-slot card-list end-slot)
  (cond ((= start-slot end-slot) #f)
        ((free? end-slot) (move-to-cell start-slot card-list end-slot))
	((home? end-slot) (and
			   (move-to-home card-list end-slot)
			   (plait-to-edge start-slot)))
	((edge? end-slot) (move-to-cell start-slot card-list end-slot))
	(else #f)))

(define (button-clicked slot)
  (if (= slot deck)
      (flip-stock deck stock 2)
      #f))

;; On double-click, move a card (other than the deck) to a home slot, or
;; else move a ``stock'' (waste-slot) card to a tableau (edge or
;; ``free'') slot
(define (button-double-clicked source) 
  (let ((dc-slots (list home-1 home-3 home-5 home-7 home-2 home-4 home-6 home-8
               free-4 free-8 free-3 free-7 free-2 free-6 free-1 free-5 
               edge-2 edge-4 edge-1 edge-3)))
    (if (not (home? source)) 
	(let ((valid-slot (find-valid-move source dc-slots))) 
	  (if valid-slot
	      (if (home? valid-slot)
		  (begin
		    (move-to-home (list (remove-card source)) valid-slot)
		    (plait-to-edge source)
		    #t)
		  (begin
		    (add-cards! valid-slot (list (remove-card source)))
		    #t)) 
	      #f)) 
	#f)))

;; Helper for double-click: find the first valid move to a slot in slot-list.
;; Any slot except deck can be moved to home.  Waste-pile cards can be moved 
;; to the tableau (edge or free slots).  
;; TODO: This should really be two separate functions, since the result is
;;     used differently, depending on whether it's a home slot or not.
(define (find-valid-move source slot-list)
  (and (not (null? slot-list))
       (not (empty-slot? source))
       (let ((target (car slot-list)))
            (cond ((and (home? target)
                        (not (empty-slot? source))
                        (move-possible? (get-top-card source) target))
                     target)
                  ((and  (or (free? target) (edge? target))
                        (= source stock)
                        (empty-slot? target))
                     target)
                  (else 
                        (find-valid-move source (cdr slot-list)))))))

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

;; Check for, in order:
;; 1) A move to the fields.
;; 2) A card can be dealt from the deck
;; 3) A card can be moved from the stock to the edges ot tableau.
(define (get-hint)
  (or (get-valid-moves '(1 0 2 19 20 9 5 6 7 8 13 14 15 16)
		       '(3 4 11 12 17 18 21 22))
      (deal-possible?)
      (if (find-valid-move stock '(0 2 5 6 7 8 13 14 15 16 19 20))
	  (hint-move stock 1 (find-valid-move stock '(0 2 5 6 7 8 13 14 15 16 19 20)))
	  #f)))

(define (game-cont)
  (and (not (game-won))
       (give-status-message)
       (get-hint)))
       
(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature scores-disabled)

(set-lambda new-game button-pressed button-released button-clicked
	    button-double-clicked game-cont game-won get-hint
	    get-options apply-options timeout droppable?)

;;; plait.scm ends here
