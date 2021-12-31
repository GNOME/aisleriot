; AisleRiot - escalator.scm
; Copyright (C) 2001, 2003 Rosanna Yuen <zana@webwynk.net>
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

(define covering-vector '#(
   (2 . (3 . 4))
   (3 . (5 . 6))
   (4 . (6 . 7))
   (5 . (8 . 9))
   (6 . (9 . 10))
   (7 . (10 . 11))
   (8 . (12 . 13))
   (9 . (13 . 14))
   (10 . (14 . 15))
   (11 . (15 . 16))
   (12 . (17 . 18))
   (13 . (18 . 19))
   (14 . (19 . 20))
   (15 . (20 . 21))
   (16 . (21 . 22))
   (17 . (23 . 24))
   (18 . (24 . 25))
   (19 . (25 . 26))
   (20 . (26 . 27))
   (21 . (27 . 28))
   (22 . (29 . 28))))

(define (covering slot-id) (if (and (<= slot-id 22)
				    (>= slot-id 2))
			       (cdr (vector-ref covering-vector (- slot-id 2)))))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())

  (add-blank-slot)
  (add-normal-slot '())

  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-blank-slot)
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-blank-slot)
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (set! HORIZPOS (+ HORIZPOS 0.5))
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)
  (set! VERTPOS (- VERTPOS 0.5))
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (deal-cards-face-up 0 '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
			    19 20 21 22 23 24 25 26 27 28 29))

  (give-status-message)

  (list 7 4))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " "
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (available? slot-id)
       (= (length card-list) 1)))

(define (available? slot-id)
  (cond ((or (= slot-id 0)
	     (= slot-id 1))
	 #f)
	((<= slot-id 22) ; Last coverable cell
	 (and (empty-slot? (car (covering slot-id)))
	      (empty-slot? (cdr (covering slot-id)))))))

(define (droppable? start-slot card-list end-slot)
  (and (= end-slot 1)
       (not (empty-slot? 1))
       (or (= (get-value (get-top-card 1))
	      (+ 1 (get-value (car card-list))))
	   (= (+ 1 (get-value (get-top-card 1)))
	      (get-value (car card-list)))
	   (and (= king (get-value (get-top-card 1)))
		(= ace (get-value (car card-list))))
	   (and (= ace (get-value (get-top-card 1)))
		(= king (get-value (car card-list)))))))

(define (button-released start-slot card-list end-slot)
  (if (droppable? start-slot card-list end-slot)
      (begin (add-to-score! 1)
	     (move-n-cards! start-slot end-slot card-list))
      #f))

(define (play-card slot-id)
  (cond ((= slot-id 0)
	 (if (not (empty-slot? 0))
	     (deal-cards-face-up 0 '(1))
	     #f))
	((and (not (= slot-id 1))
	      (not (empty-slot? slot-id))
	      (available? slot-id)
	      (not (empty-slot? 1))
	      (or (= (get-value (get-top-card 1))
		     (+ 1 (get-value (get-top-card slot-id))))
		  (= (+ 1 (get-value (get-top-card 1)))
		     (get-value (get-top-card slot-id)))
		  (and (= king (get-value (get-top-card 1)))
		       (= ace (get-value (get-top-card slot-id))))
		  (and (= ace (get-value (get-top-card 1)))
		       (= king (get-value (get-top-card slot-id))))))
	 (and (add-to-score! 1)
	      (deal-cards slot-id '(1))))
	(#t #f)))

(define (dealable?)
  (not (empty-slot? 0)))

(define (do-deal-next-cards)
  (play-card 0))

;; Single-clicking isn't sane in click-to-move more, so we mostly ignore it 
;; in that case.
(define (button-clicked slot-id)
  (if (and (click-to-move?) 
	   (> slot-id 1))
      #f
      (play-card slot-id)))

(define (button-double-clicked slot-id)
  (play-card slot-id))

(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (empty-slot? 2))

(define (playable? check-slot)
  (if (or (> check-slot 29)
	  (empty-slot? 1))
      #f
      (if (and (not (empty-slot? check-slot))
	       (available? check-slot)
	       (or (= (get-value (get-top-card 1))
		      (+ 1 (get-value (get-top-card check-slot))))
		   (= (+ 1 (get-value (get-top-card 1)))
		      (get-value (get-top-card check-slot)))
		   (and (= king (get-value (get-top-card 1)))
			(= ace (get-value (get-top-card check-slot))))
		   (and (= ace (get-value (get-top-card 1)))
			(= king (get-value (get-top-card check-slot)))0)))
	  (hint-move check-slot 1 1)
	  (playable? (+ 1 check-slot)))))

(define (dealable?)
  (and (not (empty-slot? 0))
       (list 0 (G_"Deal a card"))))

(define (get-hint)
  (or (playable? 2)
      (dealable?)))

(define (get-options) 
  #f)

(define (apply-options options) 
  #f)

(define (timeout) 
  #f)

(set-features droppable-feature dealable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable? dealable?)
