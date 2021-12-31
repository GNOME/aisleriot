; AisleRiot - golf.scm
; Copyright (C) 2001 Rosanna Yuen <zana@webwynk.net>
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

(define extend-waste #t)

(define stock 0)
(define waste 1)
(define tableau '(2 3 4 5 6 7 8))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  ;; Stock
  (add-normal-slot DECK)

  ;; Waste
  (if extend-waste
      ;; extended slot looks best but gets too big,
      ;; use part extended slot instead
      (add-partially-extended-slot '() right 26)
      ;; normal waste, only last card is shown
      (add-normal-slot '()))

  (add-carriage-return-slot)

  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  ;; deal five full rows to the tableau
  (deal-cards-face-up stock tableau)
  (deal-cards-face-up stock tableau)
  (deal-cards-face-up stock tableau)
  (deal-cards-face-up stock tableau)
  (deal-cards-face-up stock tableau)

  (give-status-message)

  (list 7 3))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (format #f (G_"Stock left: ~a") (number->string (length (get-cards stock)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id waste)
       (= (length card-list) 1)))

(define (droppable? start-slot card-list end-slot)
  (and (= end-slot waste)
       (not (empty-slot? waste))
       (let ((waste-value (get-value (get-top-card waste)))) 
	 (and (not (= waste-value king))
	      (or (= (get-value (car card-list))
		     (+ 1 waste-value))
		  (= (+ 1 (get-value (car card-list)))
		     waste-value))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (add-to-score! 1)))

(define (button-clicked slot-id)
  (or (and (= slot-id stock)
	   (not (empty-slot? slot-id))
	   (deal-cards-face-up stock (list waste)))
      (and (> slot-id waste)
	   (not (empty-slot? slot-id))
	   (not (empty-slot? waste))
	   (let ((waste-value (get-value (get-top-card waste)))) 
	     (and (not (= waste-value king))
		  (or (= (get-value (get-top-card slot-id))
			 (+ 1 waste-value))
		      (= (+ 1 (get-value (get-top-card slot-id)))
			 waste-value))
		  (deal-cards slot-id (list waste))
		  (add-to-score! 1))))))

(define (button-double-clicked slot-id)
  (button-clicked slot-id))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 2)
       (empty-slot? 3)
       (empty-slot? 4)
       (empty-slot? 5)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)))

(define (check-slots slot)
    (cond ((or (empty-slot? waste)
	       (= (get-value (get-top-card waste)) king)
	       (= slot 9))
	   #f)
	  ((let ((waste-value (get-value (get-top-card waste)))) 
	     (and (not (empty-slot? slot))
		  (or (= (get-value (get-top-card slot))
			 (+ 1 waste-value))
		      (= (+ 1 (get-value (get-top-card slot)))
			 waste-value))))
	     (hint-move slot 1 waste))
	    (#t (check-slots (+ 1 slot)))))

(define (dealable?)
  (and (not (empty-slot? stock))
       (list 0 (G_"Deal another card"))))

(define (get-hint)
  (or (check-slots (car tableau))
      (dealable?)))

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
