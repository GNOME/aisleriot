; AisleRiot - napoleons_tomb.scm
; Copyright (C) 2007 Kimmo Karlsson <kimmo.karlsson@gmail.com>
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


;;;;
;; Settings:
;;
;;;;


;;
;; True if dealing three cards at a time
;;
(define deal-three #f)


;;
;; Number of redeals left
;;
(define max-redeals 0)


;;
;; True if automatically playing cards that fit
;;
(define autoplay #f)


;;;;
;; Table set up:
;;
;;;;
(define stock 0)
(define waste 1)
(define center-slot 6)
(define corner-slots '(2 4 8 10))
(define reserve-slots '(3 5 7 9))


;;;;
;; Functions:
;;
;;;;


;;;;
;; Sets up the table.
;;
;; Returns: tuple of playing area size: {width, height} (in card-slots)
;;;;
(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)      ;; SLOT 0 - deck
  ;; SLOT 1 - turned deck
  (if deal-three
      (add-partially-extended-slot '() right 3)
      (add-normal-slot '() 'waste))
  (add-blank-slot)

  (add-normal-slot '() 'foundation)       ;; SLOT 2 - upper left
  (add-normal-slot '() 'reserve)       ;; SLOT 3 - top
  (add-normal-slot '() 'foundation)       ;; SLOT 4 - upper-right
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'reserve)       ;; SLOT 5 - left
  (add-normal-slot '() 'foundation)       ;; SLOT 6 - center
  (add-normal-slot '() 'reserve)       ;; SLOT 7 - right
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)       ;; SLOT 8 - lower left
  (add-normal-slot '() 'reserve)       ;; SLOT 9 - bottom
  (add-normal-slot '() 'foundation)       ;; SLOT 10 - lower right

  (give-status-message)
  
  ;; window is 6x3 cards
  (list 6 3))


;;;;
;; Defines which slots contain draggable cards.
;;
;; Returns: true or false 
;;;;
(define (button-pressed slot-id card-list)
  (and (member slot-id (append (list waste) reserve-slots))
       (not (null? card-list))))


;;;;
;; Checks if the given move is valid.
;;
;; Params:
;;   - start: start slot
;;   - pcard: card begin played
;;   - end: end slot
;;
;; Returns: true if card is valid
;;;;
(define (valid-card? start pcard end)
  (and (not (= start end))
       (or
	;; putting card to place-holder slot
	(and (empty-slot? end) (member end reserve-slots))
	;; putting card to the center slot
	(and (= end center-slot)
	     (or (and (= (get-value pcard) 6) (or (empty-slot? end) (= 1 (get-value (get-top-card end)))))
		 (and (not (empty-slot? end)) (= (- (get-value (get-top-card 6)) (get-value pcard)) 1))))
	;; putting card to a corner slot
	(and (member end corner-slots)
	     (or (and (empty-slot? end) (= (get-value pcard) 7))
		 (and (not (empty-slot? end)) (= (- (get-value pcard) (get-value (get-top-card end))) 1)))))))


;;;;
;; Checks if the given card can be dropped to the given slot.
;;
;; Returns: true if card is valid
;;;;
(define (droppable? start-slot card-list end-slot)
  (and (not (null? card-list))
       (= 1 (length card-list))
       (valid-card? start-slot (car card-list) end-slot)))


;;;;
;; Drops the given card to the given slot if possible.
;;
;; Returns: true if card is moved
;;;;
(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (begin (move-n-cards! start-slot end-slot card-list)
	      (if (or (= center-slot end-slot) 
		      (member end-slot corner-slots))
		  (add-to-score! 1))
	      (try-to-autoplay))))


;;;;
;; Handles a mouse click of the given slot.
;;
;; Returns: true or false
;;;;
(define (button-clicked slot-id) 
  (and (= stock slot-id)
       (flip-stock stock waste max-redeals
		   (if deal-three 3 1))
       (try-to-autoplay)))


;;;;
;; Moves the given card from the given start slot 
;; to the given end slot if the move is valid.
;;
;; Params:
;;   - start: start slot id
;;   - pcard: the card at the top of the start slot
;;   - end: end slot id
;;
;; Returns: true if card moved, false otherwise
;;;;
(define (move-if-valid start pcard end)
  (and (valid-card? start pcard end)
       (begin (move-n-cards! start end (list pcard))
	      (remove-card start)
	      (add-to-score! 1)
	      #t)))


;;;;
;; Moves the given card if the end slot is empty.
;;
;; Returns: true if card moved, false otherwise
;;;;
(define (move-if-empty start pcard end)
  (and (empty-slot? end)
       (begin (move-n-cards! start end (list pcard))
	      (remove-card start) #t)))


;;;;
;; Moves the card at the given slot to its final place if possible.
;;
;; Returns: true if a card moved, false if no card moved
;;;;
(define (autoplay-slot slot-id try-empties)
  (and (not (empty-slot? slot-id))
       (let ((c (get-top-card slot-id)))
	 (or (and (or (= waste slot-id) (member slot-id reserve-slots))
		  (or (move-if-valid slot-id c 6)
		      (move-if-valid slot-id c 2)
		      (move-if-valid slot-id c 4)
		      (move-if-valid slot-id c 8)
		      (move-if-valid slot-id c 10)))
	     (and try-empties
		  (eq? waste slot-id)
		  (or (move-if-empty slot-id c 3)
		      (move-if-empty slot-id c 5)
		      (move-if-empty slot-id c 7)
		      (move-if-empty slot-id c 9)))))))


;;;;
;; Handles a double-click of the given slot.
;;
;; Returns: true or false
;;;;
(define (button-double-clicked slot-id)
  (and (autoplay-slot slot-id #t)
       (try-to-autoplay)))


;;;;
;; Tries to autoplay any playable card on the table.
;;
;; Returns: true
;;;;
(define (try-to-autoplay)
  (define (autoplay-loop)
    (if (or (autoplay-slot waste #f)
	    (autoplay-slot 3 #f)
	    (autoplay-slot 5 #f)
	    (autoplay-slot 7 #f)
	    (autoplay-slot 9 #f))
	(delayed-call autoplay-loop) #f))
  (or (and autoplay (delayed-call autoplay-loop)) #t))


;;;;
;; Checks if the game is still continuable.
;;
;; Returns: true or false
;;;;
(define (game-continuable)
  (give-status-message)
  (or (valid-move? waste)
      (valid-move? 3)
      (valid-move? 5)
      (valid-move? 7)
      (valid-move? 9)
      (not (empty-slot? stock))
      (and (not (game-won)) (< FLIP-COUNTER max-redeals))))


;;;;
;; Checks if there is a moveable card at the top of the given slot.
;;
;; Returns: true or false
;;;;
(define (valid-move? slot-id)
  (and (not (empty-slot? slot-id))
       (let ((c (get-top-card slot-id)))
	 (or (empty-slot? 3)
	     (empty-slot? 5)
	     (empty-slot? 7)
	     (empty-slot? 9)
	     (valid-card? slot-id c 2)
	     (valid-card? slot-id c 4)
	     (valid-card? slot-id c 6)
	     (valid-card? slot-id c 8)
	     (valid-card? slot-id c 10)))))


;;;;
;; Checks if the player has finished the game successfully.
;;
;; Returns: true or false
;;;;
(define (game-won)
  (and (empty-slot? stock) 
       (empty-slot? waste)
       (empty-slot? 3)
       (empty-slot? 5)
       (empty-slot? 7)
       (empty-slot? 9)))


(define (get-reserve-hint-from from-slot to-slots)
  (cond
   ((null? to-slots) #f)
   ((empty-slot? from-slot) #f)
   ((valid-card? from-slot (get-top-card from-slot) (car to-slots))
    (hint-move from-slot 1 (car to-slots)))
   (#t (get-reserve-hint-from from-slot (cdr to-slots)))))

(define (get-reserve-hint from-slots to-slots)
  (if (null? from-slots)
      #f
      (or (get-reserve-hint-from (car from-slots) to-slots)
          (get-reserve-hint (cdr from-slots) to-slots))))

;;;;
;; Returns a hint for the current situation.
;;
;; Returns: list with zero and a hint string
;;;;
(define (get-hint)
  (or (get-reserve-hint (cons waste reserve-slots) (cons center-slot corner-slots))
     	(list 0 (G_"Deal a new card from the deck"))))


;;;;
;; Sets the status bar message.
;;
;; Returns: void
;;;;
(define (give-status-message)
  ;;
  (define (get-redeals-string)
    (if (not deal-three) ""
	(string-append (G_"Redeals left:") " "
		       (number->string (- max-redeals FLIP-COUNTER)))))
  ;;
  (define (get-stock-string)
    (string-append (G_"Stock left:") " " 
		   (number->string (length (get-cards stock)))))

  (set-statusbar-message (string-append (get-stock-string)
					"   "
					(get-redeals-string))))


;;;;
;; Lists the options.
;;
;; Returns: list of names for options and their current values
;;;;
(define (get-options) 
  (list 'begin-exclusive 
	(list (G_"Three card deals") deal-three)
	(list (G_"Single card deals") (not deal-three))
	'end-exclusive
	(list (G_"Autoplay") autoplay)))


;;;;
;; Sets new values for options from the given list.
;;
;; Params:
;;   - options: list of values for the options
;; Returns: void
;;;;
(define (apply-options options) 
  (set! deal-three (cadr (list-ref options 1)))
  (set! autoplay (cadr (list-ref options 4)))
  (set! max-redeals (if deal-three 2 0)))


;;;;
;; Checks if the time has run out.
;;
;; Returns: true or false
;;;;
(define (timeout) #f)


; droppable? is provided
(set-features droppable-feature)


;;;;
;;
;; Main. Set callback functions.
;;
;;;;
(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-continuable game-won get-hint get-options apply-options timeout droppable?)

