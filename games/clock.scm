; AisleRiot - clock.scm
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

(use-modules (aisleriot interface) (aisleriot api))

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
       (= (length card-list) 1)
       (not (= king (get-value (car card-list))))))

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

(define (droppable? start-slot card-list end-slot)
  (transaction-good? end-slot card-list))

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

(define (all-cards-visible cards)
  (if (eq? cards '())
      #t
      (and (is-visible? (car cards))
           (all-cards-visible (cdr cards)))))

(define (all-slots-visible first-slot)
  (if (= first-slot 13)
      #t
      (and (all-cards-visible (get-cards first-slot))
           (all-slots-visible (+ first-slot 1)))))

(define (game-won)
  (all-slots-visible 0))

(define (game-over)
  (not (and (is-visible? (car (reverse (get-cards 6))))
	    (= (get-value (get-top-card 6)) king))))

(define (nth-item list n)
  (if (= 0 n)
      (car list)
      (nth-item (cdr list) (- n 1))))

(define (get-hint)
  (list 0 
	(nth-item 
	 (list
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
               (G_"Just because a crosswalk looks like a hopscotch board doesn't mean it is one")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Look both ways before you cross the street")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Have you read the help file?")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Odessa is a better game.  Really.")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Tourniquets are not recommended unless in the direst emergency")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"I could sure use a backrub right about now...")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Monitors won't give you Vitamin D -- but sunlight will...")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"If you're ever lost and alone in the woods, hug a tree")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Fishing wire makes bad dental floss")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Consistency is key")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"When without a stapler, a staple and a ruler will work")
               ; Translators: This is one of the sentences that are used when the user wants to get a hint. Since the 'clock' game is a joke in itself, the sentence it nonsensical and/or a joke. So you can substitute anything you like here; you don't have to translate the original sentence!
	       (G_"Never blow in a dog's ear"))
	 (aisleriot-random 12))))

(define (get-options) #f)

(define (apply-options options) #f)

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released button-clicked button-double-clicked game-over game-won get-hint get-options apply-options timeout droppable?)
