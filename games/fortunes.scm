; AisleRiot - fortunes.scm
; Copyright (C) 1998, 2003 Rosanna Yuen <rwsy@mit.edu>
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

(use-modules (aisleriot interface) (aisleriot api) (ice-9 format))


(define (new-game)
  (initialize-playing-area)
  (set-ace-high)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)

  (add-blank-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (give-status-message)

  (list 6 3)
)

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and card-list
       (> slot-id 0)
       (or (empty-slot? 1)
	   (empty-slot? 2)
	   (empty-slot? 3)
	   (empty-slot? 4))))

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (begin
        (move-n-cards! start-slot end-slot card-list)
        #t)
      #f))

(define (droppable? start-slot card-list end-slot)
  (and (member end-slot '(1 2 3 4))
       (not (= end-slot start-slot))
       (empty-slot? end-slot)))

(define (removable? slot-id reason)
  (if (= slot-id reason)
      (if (= reason 4)
	  #f
	  (removable? slot-id (+ reason 1)))
      (if (and (not (empty-slot? reason))
	       (= (get-suit (get-top-card slot-id))
		  (get-suit (get-top-card reason)))
	       (< (get-value (get-top-card slot-id))
		  (get-value (get-top-card reason))))
	  (begin
	    (remove-card slot-id)
	    (add-to-score! 1))
	  (if (= reason 4)
	      #f
	      (removable? slot-id (+ reason 1))))))

(define (button-clicked slot-id)
  (if (empty-slot? slot-id)
      #f
      (if (= slot-id 0)
	  (begin 
	    (deal-cards 0 '(1 2 3 4))
	    (flip-top-card 1)
	    (flip-top-card 2)
	    (flip-top-card 3)
	    (flip-top-card 4))
	  (removable? slot-id 1))))
  
(define (button-double-clicked slot)
  (button-clicked slot))     
	  
(define (game-won)
  (and (empty-slot? 0)
       (= 1 (length (get-cards 1)))
       (= 1 (length (get-cards 2)))
       (= 1 (length (get-cards 3)))
       (= 1 (length (get-cards 4)))))
     
(define (game-over)
  (give-status-message)
  (not (and (empty-slot? 0)
	    (and (not (empty-slot? 1))
		     (not (empty-slot? 2))
		     (not (empty-slot? 3))
		     (not (empty-slot? 4))
		     (not (= (get-suit (get-top-card 1))
			     (get-suit (get-top-card 2))))
		     (not (= (get-suit (get-top-card 1))
			     (get-suit (get-top-card 3))))		
		     (not (= (get-suit (get-top-card 1))
			     (get-suit (get-top-card 4))))
		     (not (= (get-suit (get-top-card 2))
			     (get-suit (get-top-card 3))))
		     (not (= (get-suit (get-top-card 2))
			     (get-suit (get-top-card 4))))
		     (not (= (get-suit (get-top-card 3))
			     (get-suit (get-top-card 4))))))))

(define (check-hint slot1 slot2)
  (if (> slot2 4)
      #f
      (if (and (not (empty-slot? slot1))
	       (not (empty-slot? slot2))
	       (eq? (get-suit (get-top-card slot1))
		    (get-suit (get-top-card slot2))))
	  (if (< (get-value (get-top-card slot1))
		 (get-value (get-top-card slot2)))
	      (hint-remove-top-card slot1)
	      (hint-remove-top-card slot2))
	  (check-hint slot1 (+ 1 slot2)))))

(define (get-hint)
  (or (check-hint 1 2)
      (check-hint 2 3)
      (check-hint 3 4)
      (if (and (or (empty-slot? 1)
		   (empty-slot? 2)
		   (empty-slot? 3)
		   (empty-slot? 4))
	       (or (and (not (empty-slot? 1))
			(> (length (get-cards 1)) 1))
		   (and (not (empty-slot? 2))
			(> (length (get-cards 2)) 1))
		   (and (not (empty-slot? 3))
			(> (length (get-cards 3)) 1))
		   (and (not (empty-slot? 4))
			(> (length (get-cards 4)) 1))))
	  (list 0 (G_"Consider moving something into an empty slot"))
	  #f)
      (if (not (empty-slot? 0))
	  (list 0 (G_"Deal another round"))
	  #f)))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
