; AisleRiot - agnes.scm
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

(use-modules (aisleriot interface) (aisleriot api) (ice-9 format))

(define BASE-VAL 0)

(define stock 0)
(define foundation '(1 2 3 4))
(define tableau '(5 6 7 8 9 10 11))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-deck)
  (shuffle-deck)
  
  (add-normal-slot DECK 'stock)
  (add-blank-slot)
  (add-blank-slot)

  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)

  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (deal-cards 0 '(5 6 7 8 9 10 11 6 7 8 9 10 11 7 8 9 10 11 8 9 10 11
		    9 10 11 10 11 11))

  (map flip-top-card '(5 6 7 8 9 10 11))

  (deal-cards-face-up 0 '(1))

  (add-to-score! 1)
  (set! BASE-VAL (get-value (get-top-card 1)))

  (give-status-message)
  (dealable-set-sensitive (dealable?))
  
  (list 7 4))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string)
					"   "
					(get-base-string))))

(define (get-base-string)
  (cond ((and (> BASE-VAL 1)
	      (< BASE-VAL 11))
	 (format #f (G_"Base Card: ~a") (number->string BASE-VAL)))
	((= BASE-VAL 1)
	 (G_"Base Card: Ace"))
	((= BASE-VAL 11)
	 (G_"Base Card: Jack"))
	((= BASE-VAL 12)
	 (G_"Base Card: Queen"))
	((= BASE-VAL 13)
	 (G_"Base Card: King"))
	(#t "")))

(define (get-stock-no-string)
  (if (> (length (get-cards 0)) 1)
      (string-append (G_"Stock left:") " "
		     (number->string (length (get-cards 0))))
      (string-append (G_"Stock left: 0")))) 

(define (check-straight-descending-list-base-low card-list)
  (or (< (length card-list) 2)
      (and (= (get-value (car card-list)) king)
	   (= (get-value (cadr card-list)) ace)
	   (not (= BASE-VAL ace))
	   (check-straight-descending-list-base-low (cdr card-list)))
      (and (= (get-value (car card-list)) (- (get-value (cadr card-list)) 1))
	   (not (= BASE-VAL (get-value (cadr card-list))))
	   (check-straight-descending-list-base-low (cdr card-list)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (is-visible? (car (reverse card-list)))
       (check-same-color-list card-list)
       (check-straight-descending-list-base-low card-list)))

(define (droppable? start-slot card-list end-slot)
  (cond ((= start-slot end-slot)
	 #f)
	((and (> end-slot 0)
	      (< end-slot 5))
	 (and (= (length card-list) 1)
	      (or (and (empty-slot? end-slot)
		       (= (get-value (car card-list))
			  BASE-VAL))
		  (and (not (empty-slot? end-slot))
		       (= (get-suit (car card-list))
			  (get-suit (get-top-card end-slot)))
		       (or (= (get-value (car card-list))
			      (+ 1 (get-value (get-top-card end-slot))))
			   (and (= (get-value (car card-list)) ace)
				(= (get-value (get-top-card end-slot)) king)))))))
	((> end-slot 4)
	 (and (not (empty-slot? end-slot))
	      (eq? (is-red? (car card-list))
		   (is-red? (get-top-card end-slot)))
	      (or (= (get-value (car (reverse card-list)))
		     (- (get-value (get-top-card end-slot)) 1))
		  (and (= (get-value (car (reverse card-list))) king)
		       (= (get-value (get-top-card end-slot)) ace)))))
	(#t #f)))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (or (> start-slot 4)
           (add-to-score! -1))
       (or (> end-slot 4)
           (add-to-score! 1))
       (or (empty-slot? start-slot)
	   (make-visible-top-card start-slot))))

(define (check-slot-and-deal slot)
  (if (and (not (empty-slot? 0))
	   (< slot 12))
      (and (deal-cards-face-up 0 (list slot))
	   (check-slot-and-deal (+ 1 slot)))))

(define (do-deal-next-cards)
  (and (dealable?)
       (check-slot-and-deal 5)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (do-deal-next-cards)))

(define (dealable?)
  (not (empty-slot? 0)))

(define (check-dc slot f-slot just-checking?)
  (cond ((= f-slot 5)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card slot))
		 (get-suit (get-top-card f-slot)))
	      (or (= (get-value (get-top-card slot))
		     (+ 1 (get-value (get-top-card f-slot))))
		  (and (= (get-value (get-top-card slot)) ace)
		       (= (get-value (get-top-card f-slot)) king)))
	      (or (and just-checking?
		       f-slot)
		  (and (deal-cards slot (list f-slot))
		       (add-to-score! 1)
		       (or (empty-slot? slot)
			   (make-visible-top-card slot))))))
	(#t
	 (check-dc slot (+ 1 f-slot) just-checking?))))

(define (autoplay-foundations)
  (define (autoplay-foundations-tail)
    (if (or-map button-double-clicked '(5 6 7 8 9 10 11))
        (delayed-call autoplay-foundations-tail)
        #t))
  (if (or-map button-double-clicked '(5 6 7 8 9 10 11))
      (autoplay-foundations-tail)
      #f))

(define (button-double-clicked slot-id)
  (cond ((or (and (empty-slot? slot-id)
                  (> slot-id 4))
	     (= slot-id 0))
	 #f)
	((< slot-id 5)
	 (autoplay-foundations))
	((= (get-value (get-top-card slot-id)) BASE-VAL)
	 (and (or (and (empty-slot? 1)
		       (deal-cards slot-id '(1)))
		  (and (empty-slot? 2)
		       (deal-cards slot-id '(2)))
		  (and (empty-slot? 3)
		       (deal-cards slot-id '(3)))
		  (deal-cards slot-id '(4)))
	      (add-to-score! 1)
	      (or (empty-slot? slot-id)
		  (make-visible-top-card slot-id))))
	(#t
	 (check-dc slot-id 1 #f))))

(define (game-continuable)
  (give-status-message)
  (dealable-set-sensitive (dealable?))
  (not (game-won)))

(define (game-won)
  (and (= 13 (length (get-cards 1)))
       (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))))

(define (check-to-foundation? slot)
  (cond ((= slot 12)
	 #f)
	((and (not (empty-slot? slot))
	      (= (get-value (get-top-card slot))
		 BASE-VAL))
	 (hint-move slot 1 (find-empty-slot foundation)))
	((and (not (empty-slot? slot))
	      (check-dc slot 1 #t))
	 (hint-move slot 1 (check-dc slot 1 #t)))
	(#t (check-to-foundation? (+ 1 slot)))))

(define (check-a-tableau card slot)
  (and (not (empty-slot? slot))
       (eq? (is-red? card) (is-red? (get-top-card slot)))
       (not (= (get-value (get-top-card slot)) BASE-VAL))
       (or (and (= (get-value card) king)
		(= (get-value (get-top-card slot)) ace))
	   (= (+ (get-value card) 1)
	      (get-value (get-top-card slot))))))

(define (strip card-list)
  (cond ((< (length card-list) 2)
	 (car card-list))
	((or (not (is-visible? (car (reverse card-list))))
;	     (eq? (is-red? (car (reverse card-list)))
;		  (is-black? (car card-list)))
	     (not (check-same-color-list card-list))
	     (not (check-straight-descending-list-base-low card-list)))
	 (strip (reverse (cdr (reverse card-list)))))
	(#t (car (reverse card-list)))))

(define (check-to-tableau? slot1 slot2)
  (cond ((= slot1 12)
	 #f)
	((or (= slot2 12)
	     (empty-slot? slot1))
	 (check-to-tableau? (+ 1 slot1) 5))
	((and (not (= slot1 slot2))
	      (check-a-tableau (strip (get-cards slot1)) slot2))
	 (hint-move slot1 (find-card slot1 (strip (get-cards slot1))) slot2))
	(#t (check-to-tableau? slot1 (+ 1 slot2)))))


(define (check-deal?)
  (and (dealable?)
       (list 0 (G_"Deal more cards"))))

(define (get-hint)
  (or (check-to-foundation? 5)
      (check-to-tableau? 5 6)
      (check-deal?)
      (list 0 (G_"Try rearranging the cards"))))

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
