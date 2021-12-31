; AisleRiot - hopscotch.scm
; Copyright (C) 1999 Rosanna Yuen <rwsy@mit.edu>
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
  (set! DECK (make-deck-list-ace-low 1 5 club))
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())

  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)
  (add-extended-slot '() down)

  (add-card! 2 (make-visible (make-card ace club)))
  (add-card! 3 (make-visible (make-card 2 club)))
  (add-card! 4 (make-visible (make-card 3 club)))
  (add-card! 5 (make-visible (make-card 4 club)))

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
  (cond ((and (> end-slot 1)
	      (< end-slot 6))
	 (= (modulo (get-value (car card-list)) 13)
	    (modulo (+ (- end-slot 1) (get-value (get-top-card end-slot))) 13)))
	((and (= start-slot 1)
	      (>= end-slot 6))
	 #t)
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (begin
        (move-n-cards! start-slot end-slot card-list)
        (if (and (> end-slot 1) (< end-slot 6))
            (add-to-score! 1)
            #t))
      #f))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (empty-slot? 1)
       (flip-stock 0 1 1)))

(define (button-double-clicked slot-id)
  #f)

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (empty-slot? 0)
       (empty-slot? 1)
       (empty-slot? 6)
       (empty-slot? 7)
       (empty-slot? 8)
       (empty-slot? 9)))

(define (check-to-foundation slot-id foundation-id)
  (cond ((or (> slot-id 9)
	     (and (> slot-id 1)
		  (< slot-id 6)))
	 #f)
	((> foundation-id 5)
	 (check-to-foundation (+ 1 slot-id) 2))
	((or (empty-slot?  slot-id)
	     (and (not (empty-slot? foundation-id))
		  (= (get-value (get-top-card foundation-id)) 13))
	     (not (= (modulo (get-value (get-top-card slot-id)) 13)
		     (modulo (+ (- foundation-id 1) 
				(get-value (get-top-card foundation-id)))
			     13))))
	 (check-to-foundation slot-id (+ 1 foundation-id)))
	(#t (hint-move slot-id 1 foundation-id ))))

(define (check-waste)
  (cond ((empty-slot? 1)
	 #f)
	((check-to-foundation 1 2)
	 (check-to-foundation 1 2))
	(#t (list 0 (G_"Move card from waste")))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 (G_"Deal another card"))))

(define (get-hint)
  (or (check-to-foundation 6 2)
      (check-waste)
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

