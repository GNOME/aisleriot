; AisleRiot - camelot.scm
; Copyright (C) 1998 Rosanna Yuen <rwsy@mit.edu>
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

(define add-stage #t)
(define fill-count 0)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)
 
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-normal-slot '())           ; Slot 0
  (add-normal-slot '())           ; Slot 1
  (add-normal-slot '())           ; Slot 2
  (add-normal-slot '())           ; Slot 3
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-normal-slot '())           ; Slot 4
  (add-normal-slot '())           ; Slot 5
  (add-normal-slot '())           ; Slot 6
  (add-normal-slot '())           ; Slot 7
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-normal-slot '())           ; Slot 8
  (add-normal-slot '())           ; Slot 9
  (add-normal-slot '())           ; Slot 10
  (add-normal-slot '())           ; Slot 11
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (set! HORIZPOS (inexact->exact  (truncate (+ HORIZPOS (/ (get-card-width) 2)))))
  (add-normal-slot '())           ; Slot 12
  (add-normal-slot '())           ; Slot 13
  (add-normal-slot '())           ; Slot 14
  (add-normal-slot '())           ; Slot 15

  (set! HORIZPOS (get-horiz-start))
  (set! VERTPOS (get-vert-start))

  (add-normal-slot DECK)          ; Slot 16
  (add-normal-slot '())           ; Slot 17
  (set! add-stage #t)
  (set! fill-count 0)

  (give-status-message)

  (list 7 4)
)

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append "Stock left:  " 
		 (number->string (length (get-cards 16)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (if (= slot-id 17)
	   (and (< fill-count 16)
		(set! add-stage #t) 
		#t)
	   (not (or add-stage
		    (= slot-id 16)
		    (= (get-value (car card-list)) king)
		    (= (get-value (car card-list)) queen)
		    (= (get-value (car card-list)) jack))))))

(define (button-released start-slot card-list end-slot)
  (if add-stage
      (and (empty-slot? end-slot) 
	   (cond ((= (get-value (car card-list)) king)
		  (or (= end-slot 0)
		      (= end-slot 3)
		      (= end-slot 12)
		      (= end-slot 15)))
		 ((= (get-value (car card-list)) queen)
		  (or (= end-slot 1)
		      (= end-slot 2)
		      (= end-slot 13)
		      (= end-slot 14)))
		 ((= (get-value (car card-list)) jack)
		  (or (= end-slot 4)
		      (= end-slot 7)
		      (= end-slot 8)
		      (= end-slot 11)))
		 (#t (not (= end-slot 16))))
	   (move-n-cards! start-slot end-slot card-list)
	   (if (< end-slot 16) (set! fill-count (+ fill-count 1)))
	   #t)
      (if (= start-slot end-slot)
	  (and (= (get-value (car card-list)) 10)
	       (set! fill-count (- fill-count 1)))	       
	  (and (not (empty-slot? end-slot))
	       (not (= end-slot 17))
	       (= 10 (+ (get-value (car card-list))
			(get-value (car (get-cards end-slot)))))
	       (remove-card end-slot)
	       (set! fill-count (- fill-count 2))))))

(define (button-clicked slot-id)  
  (if (= slot-id 16)
      (and (empty-slot? 17)
	   (or (empty-slot? 0)
	       (empty-slot? 1)
	       (empty-slot? 2)
	       (empty-slot? 3)
	       (empty-slot? 4)
	       (empty-slot? 5)
	       (empty-slot? 6)
	       (empty-slot? 7)
	       (empty-slot? 8)
	       (empty-slot? 9)
	       (empty-slot? 10)
	       (empty-slot? 11)
	       (empty-slot? 12)
	       (empty-slot? 13)
	       (empty-slot? 14)
	       (empty-slot? 15))
	   (set! add-stage #t)
	   (flip-stock 16 17 0))
      (and (not add-stage)
	   (not (empty-slot? slot-id))
	   (is-visible? (get-top-card slot-id))
	   (= 10 (get-value (get-top-card slot-id)))
	   (set! fill-count (- fill-count 1))
	   (remove-card slot-id))))

(define (button-double-clicked slot)
  #f)     

(define (game-won)
  (and (empty-slot? 16)
       (empty-slot? 17)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 9)
       (empty-slot? 10)))

(define (list-cards slot)
  (if (= slot 16) 
      '() 
      (append (if (and (not (empty-slot? slot))
		       (< (get-value (get-top-card slot)) 11)) 
		  (get-cards slot) 
		  '()) 
	      (list-cards (+ 1 slot)))))

(define (find-card-val-in-list? cards value)
  (and (not (null? cards))
       (if (= value (get-value (car cards))) 
	   (car cards)
	   (find-card-val-in-list? (cdr cards) value))))

(define (find-match cards)
  (and (not (null? cards))
       (if (= 10 (get-value (car cards))) 
	   (list 2 (get-name (car cards)) "itself") ; yuk..
	   (let ((match (find-card-val-in-list? 
			 (cdr cards)
			 (- 10 (get-value (car cards))))))
	     (if match
		 (list 1 (get-name (car cards)) (get-name match))
		 (find-match (cdr cards)))))))

(define (placeable? card)
  (cond ((= (get-value card) king)
	 (and (or (empty-slot? 0)
		  (empty-slot? 3)
		  (empty-slot? 12)
		  (empty-slot? 15))
	      "an empty corner slot"))
	 ((= (get-value card) queen)
	  (or (and (or (empty-slot? 1)
		       (empty-slot? 2))
		   "an empty top slot")
	      (and (or (empty-slot? 13)
		       (empty-slot? 14))
		   "an empty bottom slot")))
	((= (get-value card) jack)
	  (or (and (or (empty-slot? 4)
		       (empty-slot? 8))
		   "an empty left slot")
	      (and (or (empty-slot? 7)
		       (empty-slot? 11))
		   "an empty right slot")))
	(#t "an empty slot")))

(define (game-over)
  (give-status-message)
  (if (or (= fill-count 16)
	  (and (empty-slot? 16) (empty-slot? 17)))
      (begin 
	(set! add-stage #f)
	(find-match (list-cards 0)))
      (or (empty-slot? 17)
	  (placeable? (get-top-card 17)))))

(define (get-hint)
  (or (if add-stage
	  (and (not (empty-slot? 17))
	       (list 2 (get-name (get-top-card 17))
		     (placeable? (get-top-card 17))))
	  (find-match (list-cards 0)))
      (list 0 "Deal a new card from the deck")))

(define (undo-func data)
  (set-score! (car data))
  (set! add-stage (cadr data))
  (set! fill-count (caddr data)))

(define (record-move slot-id old-cards)
  (set! MOVE (list undo-func
		   (list (get-score) add-stage fill-count)
		   (snapshot-board 0 slot-id old-cards))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)


