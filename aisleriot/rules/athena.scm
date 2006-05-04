;; AisleRiot - athena.scm  -*-scheme-*- 
;; Copyright (C) Alan Horkan, 2005.  
;; based on klondike.scm
; Copyright (C) 1998, 2003 Jonathan Blandford <jrb@mit.edu>
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

;; Athena differs from Klondike only in the intial layout
;; including 1 or 3 card deal, and any other options like ...
;; Optional "King Only" enabled by default [TODO]
;; TODO rewrite in a way less redundant way and share code with Klondike
;; As seen in Pretty Good Solitaire 10  http://goodsol.com  2005.  

(define deal-three #f)

; The set up:

(define tableau '(6 7 8 9 10 11 12))
(define foundation '(2 3 4 5))
(define stock 0)
(define waste 1)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK)

  (if deal-three
      (add-partially-extended-slot '() right 3)
      (add-normal-slot '()))

  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

;;; start: the layout should be the only real difference from Klondike

;; can do this lots of different ways.  
;; can do it this way: 
  (deal-cards stock '(6 7 8 9 10 11 12)) 
  (deal-cards-face-up stock '(6 7 8 9 10 11 12)) 
  (deal-cards stock '(6 7 8 9 10 11 12)) 
  (deal-cards-face-up stock '(6 7 8 9 10 11 12)) 

;; or maybe this way  
;  (deal-cards stock tableau) 
;  (deal-cards-face-up stock tableau) 
;  (deal-cards stock tableau) 
;  (deal-cards-face-up stock tableau) 
;; or all manner of other more elegant ways which could be equally effective... 
;; which is most readable and maintainable? delete the other
  
;; this would rehide the cards, dont want that.    
;  (map flip-top-card tableau)

;;; end of changes, same as Klondike from here.  

  (give-status-message)

  (list 7 3)
)

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-redeals-string))))

(define (get-redeals-string)
  (string-append (_"Redeals left:") " "
		 (number->string (- 2 FLIP-COUNTER))))

(define (get-stock-no-string)
  (string-append (_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (or (> slot-id 1)
	   (and (= slot-id 1)
		(= (length card-list) 1)))
       (is-visible? (car (reverse card-list)))))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  (if (member start-slot foundation)
      (add-to-score! -1))
  (if (member end-slot foundation)
      (add-to-score! 1))
  (if (and (not (empty-slot? start-slot)) 
	   (member start-slot tableau))
      (make-visible-top-card start-slot))
  #t)

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (complete-transaction start-slot card-list end-slot)
  #f))

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (member end-slot tableau)
		(if (empty-slot? end-slot)
		    (= king (get-value (car (reverse card-list))))
		    (and (not (eq? (is-red? (get-top-card end-slot))
				   (is-red? (car (reverse card-list)))))
			 (= (get-value (get-top-card end-slot))
			    (+ (get-value (car (reverse card-list))) 1)))))
	   (and (member end-slot foundation)
		(= 1 (length card-list))
		(if (empty-slot? end-slot)
		    (= ace (get-value (car card-list)))
		    (and (= (get-suit (get-top-card end-slot))
			    (get-suit (car card-list)))
			 (= (get-value (get-top-card end-slot)) 
			    (- (get-value (car card-list)) 1))))))))

(define (button-clicked start-slot)
  (and (= start-slot stock)
       (flip-stock stock waste 2 (if deal-three 3 1))))

(define (button-double-clicked start-slot)
  (or (and (member start-slot foundation)
            (autoplay-foundations))
      (and (member start-slot (cons waste tableau))
	   (not (empty-slot? start-slot))
	   (let* ((card (get-top-card start-slot))
		  (suit (get-suit card))
		  (value (get-value card)))
	     (let ((end-slot 
		    (cond ((if (empty-slot? 2)
			       (= ace value)
			       (= suit (get-suit (get-top-card 2)))) 2)
			  ((if (empty-slot? 3)
			       (= ace value)
			       (= suit (get-suit (get-top-card 3)))) 3)
			  ((if (empty-slot? 4)
			       (= ace value)
			       (= suit (get-suit (get-top-card 4)))) 4)
			  ((if (empty-slot? 5)
			       (= ace value)
			       (= suit (get-suit (get-top-card 5)))) 5)
			  (#t #f))))
	       (and end-slot
		    (or (= ace value)
			(= (get-value (get-top-card end-slot)) (- value 1)))
		    (remove-card start-slot)
		    (complete-transaction start-slot (list card) end-slot)))))))
(define (autoplay-foundations)
  (define (autoplay-foundations-tail)
    (if (or-map button-double-clicked (cons waste tableau))
        (delayed-call autoplay-foundations-tail)
        #t))
  (if (or-map button-double-clicked (cons waste tableau))
      (autoplay-foundations-tail)
      #f))

; Global variables used in searching (keeping it simple):

(define card #f)
(define color 0)
(define suit 0)
(define value 0)
(define slot-id1 0)

(define (match? slot-id2)
  (and (not (empty-slot? slot-id2))
       (= suit (get-suit (get-top-card slot-id2)))
       (= value (get-value (get-top-card slot-id2)))
       (list 1 (get-name (get-top-card slot-id2)) (get-name card))))

(define (ploppable? slot-id)
  (and (not (empty-slot? slot-id))
       (set! card (get-top-card slot-id))
       (set! suit (get-suit card))
       (set! value (+ (get-value card) 1))
       (or-map match? (cons waste tableau))))

(define (is-ace? slot-id)
  (and (not (empty-slot? slot-id))
       (= ace (get-value (get-top-card slot-id)))
       (list 2 (get-name (get-top-card slot-id)) (_"an empty slot" ))))

(define (shiftable? slot-id2)
  (and (not (= slot-id2 slot-id1))
       (if (empty-slot? slot-id2)
	   (and (= value king)
		(list 2 (get-name card) (_"an empty slot")))
	   (and (= (get-value (get-top-card slot-id2)) (+ 1 value))
		(not (= (get-color (get-top-card slot-id2)) color))
		(list 1 (get-name card) (get-name (get-top-card slot-id2)))))))

(define (check-visible card)
  (and (is-visible? card) card))

(define (shiftable-iter slot-id)
  (and (not (empty-slot? slot-id))
       (let ((card-list (reverse (get-cards slot-id))))
	 (set! card (or-map check-visible card-list))
	 (set! color (get-color card))	
	 (set! value (get-value card))
	 (set! slot-id1 slot-id)
	 (and (not (and (= value king)
			(eq? card (car card-list))))
	      (or-map shiftable? tableau)))))

(define (addable? slot-id)
  (if (empty-slot? slot-id)
      (and (= (get-value card) king)
	   (list 2 (get-name card) (_"an empty slot" )))
      (and (= (get-value (get-top-card slot-id)) (+ 1 (get-value card)))
	   (not (= (get-color (get-top-card slot-id)) (get-color card)))
	   (list 1 (get-name card) (get-name (get-top-card slot-id))))))

(define (get-hint)
  (or (or-map is-ace? (cons waste tableau))
      (or-map shiftable-iter tableau)
      (and (not (empty-slot? waste))
	   (set! card (get-top-card waste))
	   (or-map addable? tableau))
      (or-map ploppable? foundation)
      (and (or (and (< FLIP-COUNTER 2)
		    (not (empty-slot? waste)))
	       (not (empty-slot? stock))) 
	   (list 0 (_"Deal a new card from the deck")))
; FIXME: need to give proper hints for this case too ...
      (and (not (and-map empty-slot? '(2 3 4 5)))
           (list 0 (_"Try moving cards down from the foundation")))
      (list 0 (_"No hint available right now"))))

(define (game-won)
  (and (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))
       (= 13 (length (get-cards 5)))))

; The hints still miss some useful reversible moves:
;
; 1) unplopping cards to assist in shifting groups,
; 2) unplopping cards to assist in plopping cards in other suits, 
; 3) shifting groups to assist in plopping & unplopping cards.
;
; so we must NOT report game-over when they run out.

(define (game-over)
  (give-status-message)
  (not (game-won)))

(define (get-options)
  (list (list (_"Three card deals") deal-three)))

(define (apply-options options)
  (set! deal-three (cadar options)))

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
