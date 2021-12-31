; AisleRiot - gypsy.scm
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

(define foundation '(1 2 3 4 5 6 7 8))

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)
  (make-standard-double-deck)
  (shuffle-deck)

  (add-normal-slot DECK 'stock)

  (add-blank-slot)

  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)

  (add-carriage-return-slot)

  (add-blank-slot)

  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (deal-cards 0 '(9 10 11 12 13 14 15 16 9 10 11 12 13 14 15 16))
  (deal-cards-face-up 0 '(9 10 11 12 13 14 15 16))

  (give-status-message)


  (list 10 5))

(define (give-status-message)
  (set-statusbar-message (get-stock-no-string)))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
		 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (and (not (empty-slot? slot-id))
       (> slot-id 0)
       (not (eq? '() card-list))
       (is-visible? (car (reverse card-list)))
       (check-alternating-color-list card-list)
       (check-straight-descending-list card-list)))

(define (check-visibility slot)
  (or (empty-slot? slot)
      (is-visible? (get-top-card slot))
      (make-visible-top-card slot)))

(define (foundation-score slot-id prev-total)
  (define (current-total)
    (+ prev-total
       (* (length (get-cards slot-id)) 5)
       (if (= (length (get-cards slot-id)) 13)
           60
           0)))
  (if (= slot-id 8)
      (current-total)
      (foundation-score (+ slot-id 1) (current-total))))
       
(define (tableau-score slot-id prev-total)
  (define (cards-score cards prev-total)
    (if (< (length cards) 2)
        prev-total
        (if (and (is-visible? (car cards))
                 (is-visible? (cadr cards))
                 (not (= (get-color (car cards))
                         (get-color (cadr cards))))
                 (= (get-value (car cards))
                    (- (get-value (cadr cards)) 1)))
            (cards-score (cdr cards) (+ prev-total 2))
            (cards-score (cdr cards) prev-total))))
  (define (current-total)
    (cards-score (get-cards slot-id) prev-total))
  (if (= slot-id 16)
      (current-total)
      (tableau-score (+ slot-id 1) (current-total))))

(define (recalculate-score)
  (set-score! (+ (foundation-score 1 0)
                 (tableau-score 9 0))))

(define (droppable? start-slot card-list end-slot)
  (cond ((= end-slot start-slot)
	 #f)
	((and (> end-slot 0)
	      (< end-slot 9))
	 (if (= (length card-list) 1)
	     (cond ((empty-slot? end-slot)
		    (= (get-value (car card-list)) ace))
		   (#t
		    (and (= (get-suit (get-top-card end-slot))
			    (get-suit (car card-list)))
			 (= (get-value (car card-list))
			    (+ 1 (get-value (get-top-card end-slot)))))))
	     #f))
	((and (> end-slot 8)
	      (empty-slot? end-slot))
	 #t)
	(#t (and (> end-slot 8)
		 (eq? (is-red? (get-top-card end-slot))
		      (is-black? (car (reverse card-list))))
		 (= (get-value (get-top-card end-slot))
		    (+ 1 (get-value (car (reverse card-list)))))))))

(define (button-released start-slot card-list end-slot)
  (and (droppable? start-slot card-list end-slot)
       (move-n-cards! start-slot end-slot card-list)
       (recalculate-score)
       (check-visibility start-slot)))

(define (button-clicked slot-id)
  (and (= slot-id 0)
       (not (empty-slot? slot-id))
       (deal-cards-face-up 0 '(9 10 11 12 13 14 15 16))
       (recalculate-score)))

(define (find-empty-foundation a-slot f-slot)
  (cond ((> f-slot 8)
	 #f)
	((empty-slot? f-slot)
	 (deal-cards a-slot (list f-slot)))
	(#t (find-empty-foundation a-slot (+ 1 f-slot)))))

(define (find-foundation a-slot f-slot)
  (cond ((> f-slot 8)
	 #f)
	((and (not (empty-slot? f-slot))
	      (= (get-suit (get-top-card a-slot))
		 (get-suit (get-top-card f-slot)))
	      (= (get-value (get-top-card a-slot))
		 (+ 1 (get-value (get-top-card f-slot)))))
	 (deal-cards a-slot (list f-slot)))
	(#t (find-foundation a-slot (+ 1 f-slot)))))

(define (autoplay-foundations)
  (define (autoplay-foundations-tail)
    (if (or-map button-double-clicked '(9 10 11 12 13 14 15 16))
        (delayed-call autoplay-foundations-tail)
        #t))
  (if (or-map button-double-clicked '(9 10 11 12 13 14 15 16))
      (autoplay-foundations-tail)
      #f))

(define (button-double-clicked slot-id)
  (cond ((> slot-id 8)
         (and (not (empty-slot? slot-id))
              (or (and (= (get-value (get-top-card slot-id))
		          ace)
		       (find-empty-foundation slot-id 1)
		       (check-visibility slot-id)
		       (recalculate-score))
	          (and (find-foundation slot-id 1)
		       (check-visibility slot-id)
		       (recalculate-score)))))
	((> slot-id 0)
	 (autoplay-foundations))
	(else #f)))


(define (game-continuable)
  (give-status-message)
  (and (not (game-won))
       (get-hint)))

(define (game-won)
  (and (= (length (get-cards 1)) 13)
       (= (length (get-cards 2)) 13)
       (= (length (get-cards 3)) 13)
       (= (length (get-cards 4)) 13)
       (= (length (get-cards 5)) 13)
       (= (length (get-cards 6)) 13)
       (= (length (get-cards 7)) 13)
       (= (length (get-cards 8)) 13)))

(define (check-for-empty)
  (if (or (empty-slot? 9)
	   (empty-slot? 10)
	   (empty-slot? 11)
	   (empty-slot? 12)
	   (empty-slot? 13)
	   (empty-slot? 14)
	   (empty-slot? 15)
	   (empty-slot? 16))
      (list 0 (G_"Move a card or build of cards on to the empty slot"))
      #f))
       

(define (check-a-foundation card slot-id)
  (cond ((= slot-id 9)
	 #f)
	((and (not (empty-slot? slot-id))
	      (eq? (get-suit card)
		   (get-suit (get-top-card slot-id)))
	      (= (get-value card)
		 (+ 1 (get-value (get-top-card slot-id)))))
	 slot-id)
	(#t (check-a-foundation card (+ 1 slot-id)))))

(define (check-to-foundations? slot-id)
  (cond ((= slot-id 17)
	 #f)
	((empty-slot? slot-id)
	 (check-to-foundations? (+ 1 slot-id)))
	((= (get-value (get-top-card slot-id)) ace)
	 (hint-move slot-id 1 (find-empty-slot foundation)))
	((check-a-foundation (get-top-card slot-id) 1)
	 (hint-move slot-id 1 (check-a-foundation (get-top-card slot-id) 1)))
	(#t (check-to-foundations? (+ 1 slot-id)))))

(define (stripped card-list card)
  (if (<= (length card-list) 1)
      '()
      (if (eq? card (car card-list))
	  (cdr card-list)
	  (if (= (length card-list) 2)
	      '()
	      (stripped (cdr card-list) card)))))

(define (check-a-tableau card slot1 card-list slot2 imbedded?)
  (cond ((or (= (length card-list) 0)
	     (not (is-visible? (car card-list))))
	 #f)
	((and (not (eq? (is-red? (car card-list))
			(is-red? card)))
	      (= (+ 1 (get-value (car card-list)))
		 (get-value card)))
	 (if (or (= (length card-list) 1)
		  (eq? (is-red? (car card-list))
		       (is-red? (cadr card-list)))
		  imbedded?
		  (not (= (+ 1 (get-value (car card-list)))
			  (get-value (cadr card-list))))
		  (check-a-foundation (cadr card-list) 1)
		  (and (check-alternating-color-list (list (car card-list) (cadr card-list)))
		       (check-straight-descending-list (list (car card-list) (cadr card-list)))
		       (check-a-tableau (get-top-card slot2)
					slot1	
					(cdr card-list)
					slot2
					#t))
		  (and (> (length (get-cards slot1)) 1)
		       (check-alternating-color-list (list (get-top-card slot1) 
							   (cadr (get-cards slot1))))
		       (check-straight-descending-list (list (get-top-card slot1) 
							     (cadr (get-cards slot1))))
		       (check-a-tableau (cadr card-list)
					slot2
					(get-cards slot1)
					slot1
					#t)))
	     (hint-move slot2 (+ 1 (- (length (get-cards slot2)) (length card-list))) slot1)
	     (and (not imbedded?)
		  (> (length card-list) 1)
		  (check-alternating-color-list (list (car card-list)
						      (cadr card-list)))
		  (check-straight-descending-list (list (car card-list)
						      (cadr card-list)))
		  (check-a-tableau card 
				   slot1 
				   (cdr card-list) 
				   slot2 
				   imbedded?))))
	(imbedded? #f)
	(#t (and (> (length card-list) 1)
		 (check-alternating-color-list (list (car card-list)
						     (cadr card-list)))
		 (check-straight-descending-list (list (car card-list)
						       (cadr card-list)))
		 (check-a-tableau card slot1 (cdr card-list) slot2 imbedded?)))))

(define (check-to-tableau? slot1 slot2)
  (cond ((= slot1 17)
	 #f)
	((or (= slot2 17)
	     (empty-slot? slot1))
	 (check-to-tableau? (+ 1 slot1) 9))
	((and (not (= slot1 slot2))
	      (check-a-tableau (get-top-card slot1) 
			       slot1 
			       (get-cards slot2) 
			       slot2 
			       #f))
	 (check-a-tableau (get-top-card slot1) 
			  slot1 
			  (get-cards slot2) 
			  slot2 
			  #f))
	(#t (check-to-tableau? slot1 (+ 1 slot2)))))

(define (check-from-foundation? slot1 slot2)
  (cond ((= slot1 9)
	 #f)
	((or (= slot2 17)
	     (empty-slot? slot1))
	 (check-from-foundation? (+ 1 slot1) 9))
	(#t (or (and (not (empty-slot? slot2))
		     (check-a-tableau (get-top-card slot2) 
				      slot2 
				      (get-cards slot1)
				      slot1 
				      #f))
		(check-from-foundation? slot1 (+ 1 slot2))))))


(define (check-for-deal)
  (if (not (empty-slot? 0))
      (list 0 (G_"Deal another hand"))
      #f))

(define (get-hint)
  (or (check-to-foundations? 9)
      (check-to-tableau? 9 10)
      (check-for-empty)
      (check-for-deal)
      (check-from-foundation? 1 9)))

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
