; AisleRiot - poker.scm
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

(define shuffle-mode #f)

(define ORDERED-LIST '())

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot DECK)
  (add-normal-slot '())

  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())

  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (add-normal-slot '())
  (deal-cards-face-up 0 '(1))
  (list 8 5))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (if shuffle-mode
           (> slot-id 0)
           (= slot-id 1))))

(define (check-straight-flush card1 card2 card3 card4 card5)
  (and (check-straight)
       (check-flush card1 card2 card3 card4 card5)
       (add-to-score! 13)))

(define (caddddr some-list)
  (cadddr (cdr some-list)))

(define (check-four)
  (and (= (cadr ORDERED-LIST)
	  (caddr ORDERED-LIST)
	  (cadddr ORDERED-LIST))
       (or (= (car ORDERED-LIST)
	      (cadr ORDERED-LIST))
	   (= (caddddr ORDERED-LIST)
	      (cadr ORDERED-LIST)))
       (add-to-score! 16)))

(define (checking-straight-list num-list)
  (if (= (length num-list) 1)
      #t
      (or (and (= (+ 1 (car num-list))
		  (cadr num-list))
	       (checking-straight-list (cdr num-list)))
	  (and (= (car num-list) 1)
	       (= (cadr num-list) 10)
	       (checking-straight-list (cdr num-list))))))

(define (get-ordered-list card1 card2 card3 card4 card5)
  (set! ORDERED-LIST (sort (list (get-value card1)
				 (get-value card2)
				 (get-value card3)
				 (get-value card4)
				 (get-value card5))
			   (lambda (x y) (< x y)))))

(define (check-straight)
  (if (checking-straight-list ORDERED-LIST)
      (add-to-score! 12)
      #f))

(define (check-full)
  (and (= (car ORDERED-LIST)
	  (cadr ORDERED-LIST))
       (= (cadddr ORDERED-LIST)
	  (caddddr ORDERED-LIST))
       (or (= (car ORDERED-LIST)
	      (caddr ORDERED-LIST))
	   (= (caddddr ORDERED-LIST)
	      (caddr ORDERED-LIST)))
       (add-to-score! 10)))

(define (check-three)
  (and (or (= (car ORDERED-LIST)
	      (cadr ORDERED-LIST)
	      (caddr ORDERED-LIST))
	   (= (cadr ORDERED-LIST)
	      (caddr ORDERED-LIST)
	      (cadddr ORDERED-LIST))
	   (= (caddr ORDERED-LIST)
	      (cadddr ORDERED-LIST)
	      (caddddr ORDERED-LIST)))
       (add-to-score! 6)))

(define (check-flush card1 card2 card3 card4 card5)
  (and (= (get-suit card1)
	  (get-suit card2)
	  (get-suit card3)
	  (get-suit card4)
	  (get-suit card5))
       (add-to-score! 5)))

(define (check-two)
  (and (or (and (= (car ORDERED-LIST)
		   (cadr ORDERED-LIST))
		(= (caddr ORDERED-LIST)
		   (cadddr ORDERED-LIST)))
	   (and (= (car ORDERED-LIST)
		   (cadr ORDERED-LIST))
		(= (cadddr ORDERED-LIST)
		   (caddddr ORDERED-LIST)))
	   (and (= (cadr ORDERED-LIST)
		   (caddr ORDERED-LIST))
		(= (cadddr ORDERED-LIST)
		   (caddddr ORDERED-LIST))))
       (add-to-score! 3)))

(define (check-one)
  (and (or (= (car ORDERED-LIST)
	      (cadr ORDERED-LIST))
	   (= (cadr ORDERED-LIST)
	      (caddr ORDERED-LIST))
	   (= (caddr ORDERED-LIST)
	      (cadddr ORDERED-LIST))
	   (= (cadddr ORDERED-LIST)
	      (caddddr ORDERED-LIST)))
       (add-to-score! 1)))

(define (check-hand card1 card2 card3 card4 card5)
  (get-ordered-list card1 card2 card3 card4 card5)
  (or (check-straight-flush card1 card2 card3 card4 card5)
      (check-four)
      (check-full)
      (check-three)
      (check-flush card1 card2 card3 card4 card5)
      (check-two)
      (check-one)
      #t))

(define (check-a-horiz buffer)
  (and (not (empty-slot? (+ 2 buffer)))
       (not (empty-slot? (+ 3 buffer)))
       (not (empty-slot? (+ 4 buffer)))
       (not (empty-slot? (+ 5 buffer)))
       (not (empty-slot? (+ 6 buffer)))
       (check-hand (get-top-card (+ 2 buffer))
		   (get-top-card (+ 3 buffer))
		   (get-top-card (+ 4 buffer))
		   (get-top-card (+ 5 buffer))
		   (get-top-card (+ 6 buffer))))
  (or (= buffer 20)
      (check-a-horiz (+ buffer 5))))

(define (check-a-vert buffer)
  (and (not (empty-slot? (+ 2 buffer)))
       (not (empty-slot? (+ 7 buffer)))
       (not (empty-slot? (+ 12 buffer)))
       (not (empty-slot? (+ 17 buffer)))
       (not (empty-slot? (+ 22 buffer)))
       (check-hand (get-top-card (+ 2 buffer))
		   (get-top-card (+ 7 buffer))
		   (get-top-card (+ 12 buffer))
		   (get-top-card (+ 17 buffer))
		   (get-top-card (+ 22 buffer))))
  (or (= buffer 4)
      (check-a-vert (+ buffer 1))))

(define (check-diags)
  (and (not (empty-slot? 2))
       (not (empty-slot? 8))
       (not (empty-slot? 14))
       (not (empty-slot? 20))
       (not (empty-slot? 26))
       (check-hand (get-top-card 2)
		   (get-top-card 8)
		   (get-top-card 14)
		   (get-top-card 20)
		   (get-top-card 26)))
  (and (not (empty-slot? 6))
       (not (empty-slot? 10))
       (not (empty-slot? 14))
       (not (empty-slot? 18))
       (not (empty-slot? 22))
       (check-hand (get-top-card 6)
		   (get-top-card 10)
		   (get-top-card 14)
		   (get-top-card 18)
		   (get-top-card 22))))

(define (recalculate-score)
  (set-score! 0)
  (check-diags)
  (check-a-vert 0)
  (check-a-horiz 0))

(define (droppable? start-slot card-list end-slot)
  (and (or shuffle-mode
           (empty-slot? end-slot))
       (not (= start-slot end-slot))
       (> end-slot 1)
       (or (> start-slot 1)
           (empty-slot? end-slot))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (or (empty-slot? end-slot)
           (deal-cards end-slot (list start-slot)))
       (move-n-cards! start-slot end-slot card-list)
       (or (> start-slot 1)
           (= (length (get-cards 0)) 27)
           (deal-cards-face-up 0 '(1)))
       (recalculate-score)))

(define (button-clicked slot-id)
  #f)

(define (button-double-clicked slot-id)
  #f)

(define (all-slots-full slot-id)
  (or (= slot-id 27)
      (and (not (empty-slot? slot-id))
           (all-slots-full (+ slot-id 1)))))

(define (game-continuable)
  (or shuffle-mode
      (not (all-slots-full 2))))

(define (game-won)
  (if shuffle-mode
      (> (get-score) 119)
      (> (get-score) 74)))

(define (get-hint)
  (list 0 (G_"Place cards on to the Tableau to form poker hands")))

(define (get-options)
  (list (list (G_"Shuffle mode") shuffle-mode)))

(define (apply-options options)
  (set! shuffle-mode (cadar options)))

(define (timeout) 
  #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked
button-double-clicked game-continuable game-won get-hint get-options
apply-options timeout droppable?)
