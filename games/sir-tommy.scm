; AisleRiot - sir_tommy.scm
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

(use-modules (aisleriot interface) (aisleriot api))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)
  (add-normal-slot '() 'waste)

  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() down 'reserve)
  (add-extended-slot '() down 'reserve)
  (add-extended-slot '() down 'reserve)
  (add-extended-slot '() down 'reserve)

  (give-status-message)
  (list 7 4))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (= (length card-list) 1)
       (or (= slot-id 1)
	   (> slot-id 5))))

(define (droppable? start-slot card-list end-slot)
  (cond ((> end-slot 5)
	 (= start-slot 1))
	((> end-slot 1)
	 (or (and (= (get-value (car card-list)) ace)
		  (empty-slot? end-slot))
	     (and (not (empty-slot? end-slot))
		  (= (get-value (car card-list))
		     (+ 1 (get-value (get-top-card end-slot)))))))
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (cond ((> end-slot 5)
	      (move-n-cards! start-slot end-slot card-list))
	     ((> end-slot 1)
	      (begin
		(move-n-cards! start-slot end-slot card-list)
		(add-to-score! 1)))
	     (#t #f))))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? 0))
       (empty-slot? 1)
       (deal-cards-face-up 0 '(1))))

(define (check-top-card slot f-slot)
  (cond ((= f-slot 6)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-value (get-top-card slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (list f-slot))
	((and (= (get-value (get-top-card slot)) 
		 ace)
	      (empty-slot? f-slot))
	 (list f-slot))
	(#t (check-top-card slot (+ 1 f-slot)))))

(define (button-double-clicked slot-id)
  (and (not (empty-slot? slot-id))
       (or (= slot-id 1)
	   (> slot-id 5))
       (check-top-card slot-id 2)
       (deal-cards slot-id (check-top-card slot-id 2))
       (add-to-score! 1)))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)))

(define (check-to-foundation slot)
  (cond ((= slot 10)
	 #f)
	((= slot 2)
	 (check-to-foundation 6))
	((and (not (empty-slot? slot))
	      (check-top-card slot 2))
	 (hint-move slot 1 (car (check-top-card slot 2))))
	(#t (check-to-foundation (+ 1 slot)))))

(define (move-waste)
  (and (not (empty-slot? 1))
       (not (empty-slot? 0))
       (list 0 (G_"Move waste on to a reserve slot"))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 (G_"Deal another card"))))

(define (get-hint)
  (or (check-to-foundation 1)
      (move-waste)
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
