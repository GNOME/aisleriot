; AisleRiot - clock.scm
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

(define (new-game)
  (initialize-playing-area)
  (make-standard-deck)
  (shuffle-deck)

  (add-blank-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-extended-slot '() right)
  (add-carriage-return-slot)
  
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() right)
  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot DECK right)
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() right)
  (add-carriage-return-slot)

  (add-extended-slot '() right)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-blank-slot)
  (add-extended-slot '() right)
  (add-carriage-return-slot)

  (add-blank-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-extended-slot '() right)
  (add-blank-slot)
  (add-extended-slot '() right)

  (deal-cards 6 '(2 4 7 9 12 11 10 8 5 3 0 1 2 4 7 9 12 11 10 8 5 3 0 1 2 4 7 9 12 11 10 8 5 3 0 1 2 4 7 9 12 11 10 8 5 3 0 1 ))

  (flip-top-card 6)

  (list 9 5)
)

(define (button-pressed slot-id card-list)
  (and (= slot-id 6)
       (= (length card-list) 1)))

(define (transaction-good? end-slot card-list)
  (or (and (= end-slot 2)
	   (= ace (get-value (car card-list))))
      (and (= end-slot 4)
	   (= 2 (get-value (car card-list))))
      (and (= end-slot 7)
	   (= 3 (get-value (car card-list))))
      (and (= end-slot 9)
	   (= 4 (get-value (car card-list))))
      (and (= end-slot 12)
	   (= 5 (get-value (car card-list))))
      (and (= end-slot 11)
	   (= 6 (get-value (car card-list))))
      (and (= end-slot 10)
	   (= 7 (get-value (car card-list))))
      (and (= end-slot 8)
	   (= 8 (get-value (car card-list))))
      (and (= end-slot 5)
	   (= 9 (get-value (car card-list))))
      (and (= end-slot 3)
	   (= 10 (get-value (car card-list))))
      (and (= end-slot 0)
	   (= jack (get-value (car card-list))))
      (and (= end-slot 1)
	   (= queen (get-value (car card-list))))
      (and (= end-slot 6)
	   (= king (get-value (car card-list))))))

(define (complete-transaction card-list end-slot)
  (add-cards! end-slot card-list)
  (add-card! 6 (car (reverse (get-cards end-slot))))
  (set-cards! end-slot (reverse (cdr (reverse (get-cards end-slot)))))
  (make-visible-top-card 6)
  (if (not (= end-slot 6))
      (add-to-score! 1)
      #t))

(define (button-released start-slot card-list end-slot)
  (if (transaction-good? end-slot card-list)
      (complete-transaction card-list end-slot)
      #f))

(define (button-clicked slot-id)
  (if (and (= (get-value (get-top-card slot-id)) king)
	   (is-visible? (get-top-card slot-id)))
      (begin
	(set-cards! 6 (cons (car (reverse (get-cards 6)))
			    (reverse (cdr (reverse (get-cards 6))))))
	(make-visible-top-card 6))
      #f))

(define (button-double-clicked slot)
  (if (and (not (= slot 6))
	   (transaction-good? slot (get-cards 6)))
      (if (= slot 6)
	  (set-cards! 6 (cons (car (reverse (get-cards 6))) 
			      (reverse (cdr (reverse (get-cards 6))))))
	  (begin
	    (let ((top-card (get-top-card 6)))
	      (set-cards! 6 (cdr (get-cards 6)))
	      (complete-transaction (list top-card) slot))))
      #f))

(define (make-all-visible slot)
  (if (< slot 13)
      (begin
	(make-visible (car (get-cards slot)))
	(make-visible (cadr (get-cards slot)))
	(make-visible (caddr (get-cards slot)))
	(make-visible (cadddr (get-cards slot)))
	(make-all-visible (+ slot 1)))))


(define (game-won)
  (make-all-visible 0)
  (if (and (= (get-value (car (get-cards 2))) 1)
	   (= (get-value (cadr (get-cards 2))) 1)
	   (= (get-value (caddr (get-cards 2))) 1)
	   (= (get-value (cadddr (get-cards 2))) 1)
	   (= (get-value (car (get-cards 4))) 2)
	   (= (get-value (cadr (get-cards 4))) 2)
	   (= (get-value (caddr (get-cards 4))) 2)
	   (= (get-value (cadddr (get-cards 4))) 2)
	   (= (get-value (car (get-cards 7))) 3)
	   (= (get-value (cadr (get-cards 7))) 3)
	   (= (get-value (caddr (get-cards 7))) 3)
	   (= (get-value (cadddr (get-cards 7))) 3)
	   (= (get-value (car (get-cards 9))) 4)
	   (= (get-value (cadr (get-cards 9))) 4)
	   (= (get-value (caddr (get-cards 9))) 4)
	   (= (get-value (cadddr (get-cards 9))) 4)
	   (= (get-value (car (get-cards 12))) 5)
	   (= (get-value (cadr (get-cards 12))) 5)
	   (= (get-value (caddr (get-cards 12))) 5)
	   (= (get-value (cadddr (get-cards 12))) 5)
	   (= (get-value (car (get-cards 11))) 6)
	   (= (get-value (cadr (get-cards 11))) 6)
	   (= (get-value (caddr (get-cards 11))) 6)
	   (= (get-value (cadddr (get-cards 11))) 6)
	   (= (get-value (car (get-cards 10))) 7)
	   (= (get-value (cadr (get-cards 10))) 7)
	   (= (get-value (caddr (get-cards 10))) 7)
	   (= (get-value (cadddr (get-cards 10))) 7)
	   (= (get-value (car (get-cards 8))) 8)
	   (= (get-value (cadr (get-cards 8))) 8)
	   (= (get-value (caddr (get-cards 8))) 8)
	   (= (get-value (cadddr (get-cards 8))) 8)
	   (= (get-value (car (get-cards 5))) 9)
	   (= (get-value (cadr (get-cards 5))) 9)
	   (= (get-value (caddr (get-cards 5))) 9)
	   (= (get-value (cadddr (get-cards 5))) 9)
	   (= (get-value (car (get-cards 3))) 10)
	   (= (get-value (cadr (get-cards 3))) 10)
	   (= (get-value (caddr (get-cards 3))) 10)
	   (= (get-value (cadddr (get-cards 3))) 10)
	   (= (get-value (car (get-cards 0))) 11)
	   (= (get-value (cadr (get-cards 0))) 11)
	   (= (get-value (caddr (get-cards 0))) 11)
	   (= (get-value (cadddr (get-cards 0))) 11)
	   (= (get-value (car (get-cards 1))) 12)
	   (= (get-value (cadr (get-cards 1))) 12)
	   (= (get-value (caddr (get-cards 1))) 12)
	   (= (get-value (cadddr (get-cards 1))) 12)
	   )
      #t
      #f))

(define (game-over)
  (not (and (is-visible? (car (reverse (get-cards 6))))
	    (= (get-value (get-top-card 6)) king)
	    (make-all-visible 0))))

(define (nth-item list n)
  (if (= 0 n)
      (car list)
      (nth-item (cdr list) (- n 1))))

(define (get-hint)
  (list 0 
	(nth-item 
	 (list "Just because a crosswalk looks like a hopscotch board doesn't mean it is one" 
	       "Look both ways before you cross the street"
	       "Have you read the help file?"
	       "Odessa is a better game.  Really."
	       "Tourniquets are not recommended unless in the direst emergency"
	       "I could sure use a backrub right about now..."
	       "Monitors won't give you Vitamin E -- but sunlight will..."
	       "If you're ever lost and alone in the woods, hug a tree"
	       "Fishing wire makes bad dental floss"
	       "Consistency is key"
	       "When without a stapler, a staple and a ruler will work"
	       "Never blow in a dog's ear")
	 (random 12))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout)
